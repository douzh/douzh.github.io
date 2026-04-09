#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
使用 LightRAG 导入 10one 知识库
支持向量 + 图谱双引擎
"""

import os
import asyncio
from pathlib import Path
from lightrag import LightRAG, QueryParam
from lightrag.utils import EmbeddingFunc
from sentence_transformers import SentenceTransformer
import aiohttp
import json

# 配置
WORKING_DIR = "./10one-lightrag-memory"
SOURCE_DIR = r"d:\mycloud\0pnbase\kb-main\10one"

# LLM 配置
LLM_API_KEY = "sk-aYpw6xK9RelztcZSDcjfecrH0VvMBENg"
LLM_BASE_URL = "https://openai.imedicalai.com/ai-gateway/hos-base-platform"
LLM_MODEL = "/models/models/Qwen3-32B/"

async def llm_model_func(prompt, system_prompt=None, history_messages=[], **kwargs):
    """自定义 LLM 调用函数 - 直接调用 API"""
    url = f"{LLM_BASE_URL}/qwen3-32b/chat/completions"
    
    messages = []
    if system_prompt:
        messages.append({"role": "system", "content": system_prompt})
    messages.extend(history_messages)
    messages.append({"role": "user", "content": prompt})
    
    payload = {
        "messages": messages,
        "model": LLM_MODEL,
        "temperature": kwargs.get("temperature", 0.8),
        "stream": False
    }
    
    headers = {
        "Content-Type": "application/json",
        "Authorization": f"Bearer {LLM_API_KEY}"
    }
    
    async with aiohttp.ClientSession() as session:
        async with session.post(url, json=payload, headers=headers) as resp:
            result = await resp.json()
            return result["choices"][0]["message"]["content"]

async def embedding_func(texts: list[str]) -> list[list[float]]:
    """本地 Embedding 模型"""
    model = SentenceTransformer('all-MiniLM-L6-v2')
    embeddings = model.encode(texts, show_progress_bar=True)
    return embeddings.tolist()

def read_markdown_files(directory):
    """读取目录下所有 Markdown 文件"""
    documents = []
    md_files = list(Path(directory).glob('*.md'))
    
    print(f"找到 {len(md_files)} 个 Markdown 文件")
    
    for md_file in md_files:
        with open(md_file, 'r', encoding='utf-8') as f:
            content = f.read()
        
        documents.append({
            'filename': md_file.name,
            'content': content
        })
        print(f"  ✓ 读取: {md_file.name}")
    
    return documents

async def main():
    print("=" * 60)
    print("LightRAG 知识库导入工具")
    print("=" * 60)
    
    # 初始化 LightRAG
    print("\n📦 初始化 LightRAG...")
    rag = LightRAG(
        working_dir=WORKING_DIR,
        llm_model_func=llm_model_func,
        embedding_func=EmbeddingFunc(
            embedding_dim=384,
            max_token_size=8192,
            func=embedding_func
        )
    )
    
    # 初始化存储
    await rag.initialize_storages()
    
    # 读取文档
    print(f"\n📂 读取文档目录: {SOURCE_DIR}")
    documents = read_markdown_files(SOURCE_DIR)
    
    # 插入文档
    print(f"\n🚀 开始索引 {len(documents)} 个文档...")
    for doc in documents:
        try:
            await rag.ainsert(doc['content'])
            print(f"  ✓ 已索引: {doc['filename']}")
        except Exception as e:
            print(f"  ✗ 失败: {doc['filename']} - {e}")
    
    print("\n✅ 索引完成!")
    print(f"\n💾 数据存储在: {WORKING_DIR}")
    print("\n📊 存储结构:")
    print("  - vector/     : FAISS 向量索引")
    print("  - graph/      : NetworkX 知识图谱")
    print("  - docs.db     : SQLite 原文存储")
    
    # 测试查询
    print("\n" + "=" * 60)
    print("测试查询功能")
    print("=" * 60)
    
    test_queries = [
        "什么是混沌经?",
        "智慧生命三义是什么?",
        "六步循环法包括哪些步骤?"
    ]
    
    for query in test_queries:
        print(f"\n❓ 问题: {query}")
        try:
            result = await rag.aquery(
                query,
                param=QueryParam(mode="hybrid")
            )
            print(f"💡 回答: {result[:200]}...")
        except Exception as e:
            print(f"⚠️  查询失败: {e}")

if __name__ == "__main__":
    asyncio.run(main())
