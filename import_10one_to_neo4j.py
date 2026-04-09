#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
将 10one 目录的 Markdown 知识库导入 Neo4j 图数据库
"""

import os
import re
from neo4j import GraphDatabase
from pathlib import Path

class KnowledgeGraphImporter:
    def __init__(self, uri="bolt://localhost:7687", user="neo4j", password="neo4j"):
        self.driver = GraphDatabase.driver(uri, auth=(user, password))
        
    def close(self):
        self.driver.close()
    
    def clear_database(self):
        """清空数据库"""
        with self.driver.session() as session:
            session.run("MATCH (n) DETACH DELETE n")
            print("✓ 数据库已清空")
    
    def parse_markdown_file(self, file_path):
        """解析 Markdown 文件，提取元数据和内容结构"""
        with open(file_path, 'r', encoding='utf-8') as f:
            content = f.read()
        
        # 提取 YAML front matter
        metadata = {}
        yaml_match = re.match(r'^---\s*\n(.*?)\n---\s*\n', content, re.DOTALL)
        if yaml_match:
            yaml_content = yaml_match.group(1)
            for line in yaml_content.split('\n'):
                if ':' in line:
                    key, value = line.split(':', 1)
                    metadata[key.strip()] = value.strip()
            main_content = content[yaml_match.end():]
        else:
            main_content = content
        
        # 提取标题层级
        headings = []
        for match in re.finditer(r'^(#{1,6})\s+(.+)$', main_content, re.MULTILINE):
            level = len(match.group(1))
            title = match.group(2).strip()
            headings.append({'level': level, 'title': title})
        
        # 提取标签
        tags = metadata.get('tags', '').split(',')
        tags = [tag.strip() for tag in tags if tag.strip()]
        
        return {
            'filename': os.path.basename(file_path),
            'filepath': str(file_path),
            'title': metadata.get('title', ''),
            'type': metadata.get('type', ''),
            'tags': tags,
            'headings': headings,
            'content': main_content,  # 存储完整内容
            'content_length': len(main_content)
        }
    
    def create_document_node(self, session, doc_data):
        """创建文档节点"""
        query = """
        CREATE (d:Document {
            filename: $filename,
            filepath: $filepath,
            title: $title,
            type: $type,
            content: $content,
            content_length: $content_length,
            category: '命记'
        })
        RETURN d
        """
        result = session.run(query, **doc_data)
        return result.single()['d']
    
    def create_tag_nodes(self, session, doc_id, tags):
        """创建标签节点并建立关系"""
        for tag in tags:
            # 创建或获取标签节点
            tag_query = """
            MERGE (t:Tag {name: $tag})
            RETURN t
            """
            session.run(tag_query, tag=tag)
            
            # 建立文档与标签的关系
            rel_query = """
            MATCH (d:Document), (t:Tag)
            WHERE id(d) = $doc_id AND t.name = $tag
            CREATE (d)-[:HAS_TAG]->(t)
            """
            session.run(rel_query, doc_id=doc_id, tag=tag)
    
    def create_heading_nodes(self, session, doc_id, headings):
        """创建标题节点并建立层级关系"""
        prev_node_id = None
        
        for heading in headings:
            # 创建标题节点
            h_query = """
            CREATE (h:Heading {
                title: $title,
                level: $level
            })
            RETURN id(h) as hid
            """
            result = session.run(h_query, **heading)
            current_id = result.single()['hid']
            
            # 建立与文档的关系
            doc_rel_query = """
            MATCH (d:Document), (h:Heading)
            WHERE id(d) = $doc_id AND id(h) = $hid
            CREATE (d)-[:HAS_HEADING]->(h)
            """
            session.run(doc_rel_query, doc_id=doc_id, hid=current_id)
            
            # 建立标题间的层级关系
            if prev_node_id is not None:
                hier_query = """
                MATCH (prev:Heading), (curr:Heading)
                WHERE id(prev) = $prev_id AND id(curr) = $curr_id
                CREATE (prev)-[:NEXT]->(curr)
                """
                session.run(hier_query, prev_id=prev_node_id, curr_id=current_id)
            
            prev_node_id = current_id
    
    def create_concept_relationships(self, session):
        """创建概念之间的关系（基于文件名和引用）"""
        # 基于章节编号创建顺序关系
        query = """
        MATCH (d1:Document), (d2:Document)
        WHERE d1.filename < d2.filename 
        AND d1.category = '命记' AND d2.category = '命记'
        WITH d1, d2
        ORDER BY d1.filename, d2.filename
        WITH collect(d1) as docs
        UNWIND range(0, size(docs)-2) as i
        WITH docs[i] as d1, docs[i+1] as d2
        CREATE (d1)-[:NEXT_CHAPTER]->(d2)
        """
        session.run(query)
    
    def import_directory(self, directory_path):
        """导入整个目录"""
        print(f"开始导入目录: {directory_path}")
        
        # 获取所有 markdown 文件
        md_files = list(Path(directory_path).glob('*.md'))
        print(f"找到 {len(md_files)} 个 Markdown 文件")
        
        with self.driver.session() as session:
            for md_file in md_files:
                print(f"\n处理文件: {md_file.name}")
                
                # 解析文件
                doc_data = self.parse_markdown_file(md_file)
                
                # 创建文档节点
                doc_node = self.create_document_node(session, doc_data)
                doc_id = doc_node.id
                
                print(f"  ✓ 创建文档节点: {doc_data['title']}")
                
                # 创建标签节点
                if doc_data['tags']:
                    self.create_tag_nodes(session, doc_id, doc_data['tags'])
                    print(f"  ✓ 创建 {len(doc_data['tags'])} 个标签")
                
                # 创建标题节点
                if doc_data['headings']:
                    self.create_heading_nodes(session, doc_id, doc_data['headings'])
                    print(f"  ✓ 创建 {len(doc_data['headings'])} 个标题节点")
            
            # 创建章节间关系
            print("\n创建章节关系...")
            self.create_concept_relationships(session)
            print("  ✓ 章节关系创建完成")
        
        print(f"\n✅ 导入完成！共处理 {len(md_files)} 个文件")
    
    def get_statistics(self):
        """获取图谱统计信息"""
        with self.driver.session() as session:
            queries = {
                '文档数': "MATCH (d:Document) RETURN count(d) as count",
                '标签数': "MATCH (t:Tag) RETURN count(t) as count",
                '标题数': "MATCH (h:Heading) RETURN count(h) as count",
                '关系数': "MATCH ()-[r]->() RETURN count(r) as count"
            }
            
            print("\n📊 图谱统计:")
            print("-" * 40)
            for name, query in queries.items():
                result = session.run(query)
                count = result.single()['count']
                print(f"{name:10s}: {count}")


if __name__ == '__main__':
    # 配置
    NEO4J_URI = "bolt://localhost:7687"
    NEO4J_USER = "neo4j"
    NEO4J_PASSWORD = "neo4j123"  # 如果修改了密码，请更新这里
    SOURCE_DIR = r"d:\mycloud\0pnbase\kb-main\10one"
    
    # 创建导入器
    importer = KnowledgeGraphImporter(NEO4J_URI, NEO4J_USER, NEO4J_PASSWORD)
    
    try:
        # 清空数据库（可选，注释掉以保留现有数据）
        importer.clear_database()
        
        # 导入目录
        importer.import_directory(SOURCE_DIR)
        
        # 显示统计
        importer.get_statistics()
        
    except Exception as e:
        print(f"❌ 错误: {e}")
    finally:
        importer.close()
