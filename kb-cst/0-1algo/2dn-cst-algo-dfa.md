
# algo-DFA

DFA全称为：Deterministic Finite Automaton,即确定有穷自动机。其特征为：有一个有限状态集合和一些从一个状态通向另一个状态的边，每条边上标记有一个符号，其中一个状态是初态，某些状态是终态。但不同于不确定的有限自动机，DFA中不会有从同一状态出发的两条边标志有相同的符号。

敏感词过滤很适合用DFA算法，用户每次输入都是状态的切换，如果出现敏感词，既是终态，就可以结束判断。

构建DFA MAP

``` java

/**
 * 敏感词转map
 * @param keyWordSet
 */
public static ConcurrentHashMap addSensitiveWordToHashMap(Set<String> keyWordSet) {
    ConcurrentHashMap sensitiveWordMap = new ConcurrentHashMap(keyWordSet.size());
    String key = null;
    ConcurrentHashMap nowMap = null;
    ConcurrentHashMap<String, String> newWorMap = null;
    //迭代keyWordSet
    Iterator<String> iterator = keyWordSet.iterator();
    while(iterator.hasNext()){
        //关键字
        key = iterator.next();
        nowMap = sensitiveWordMap;
        for(int i = 0 ; i < key.length() ; i++){
            //转换成char型
            char keyChar = key.charAt(i);
            //获取
            Object wordMap = nowMap.get(keyChar);
            //如果存在该key，直接赋值
            if(wordMap != null){
                nowMap = (ConcurrentHashMap) wordMap;
            }
            else{
                //不存在则，则构建一个map，同时将isEnd设置为0，因为他不是最后一个
                newWorMap = new ConcurrentHashMap<String,String>();
                //不是最后一个
                newWorMap.put("isEnd", "0");
                nowMap.put(keyChar, newWorMap);
                nowMap = newWorMap;
            }
            //最后一个
            if(i == key.length() - 1){
                nowMap.put("isEnd", "1");
            }
        }
    }
    return sensitiveWordMap;
}

```

常用检测方法

``` java


/**
 * 检查是否存在敏感词
 * @param txt
 * @param sensitiveWordMap
 * @param matchType
 * @return true:包含
 */
@Override
public boolean isContaintSensitiveWord(String txt, Map sensitiveWordMap, int matchType){
    boolean flag = false;
    for(int i = 0 ; i < txt.length() ; i++){
        //判断是否包含敏感字符
        int matchFlag = this.checkSensitiveWord(txt, i, sensitiveWordMap, matchType);
        //大于0存在，返回true
        if(matchFlag > 0){
            flag = true;
        }
    }
    return flag;
}

/**
 * 获取文本中的敏感词
 * @param txt
 * @param sensitiveWordMap
 * @param matchType
 * @return
 */
@Override
public Set<String> getSensitiveWord(String txt, Map sensitiveWordMap, int matchType){
    Set<String> sensitiveWordList = new HashSet<String>();

    for(int i = 0 ; i < txt.length() ; i++){
        //判断是否包含敏感字符
        int length = checkSensitiveWord(txt, i, sensitiveWordMap, matchType);
        //存在,加入list中
        if(length > 0){
            sensitiveWordList.add(txt.substring(i, i+length));
            //减1的原因，是因为for会自增
            i = i + length - 1;
        }
    }

    return sensitiveWordList;
}

/**
 * 检查文字中是否包含敏感字符
 * @param text
 * @param beginIndex
 * @param sensitiveWordMap
 * @param matchType
 * @return 如果存在，则返回敏感词字符的长度，不存在返回0
 */
public int checkSensitiveWord(String text, int beginIndex, Map sensitiveWordMap, int matchType) {
    //敏感词结束标识位：用于敏感词只有1位的情况
    boolean  flag = false;
    //匹配标识数默认为0
    int matchFlag = 0;
    char word = 0;
    Map nowMap = sensitiveWordMap;
    for(int i = beginIndex; i < text.length() ; i++){
        word = text.charAt(i);
        //获取指定key
        nowMap = (Map) nowMap.get(word);
        //存在，则判断是否为最后一个
        if(nowMap != null){
            //找到相应key，匹配标识+1
            matchFlag++;
            //如果为最后一个匹配规则,结束循环，返回匹配标识数
            if("1".equals(nowMap.get("isEnd"))){
                //结束标志位为true
                flag = true;
                //最小规则，直接返回,最大规则还需继续查找
                if(SensitiveConstant.MIN_MATCH_TYPE == matchType){
                    break;
                }
            }
        }
        else{
            //不存在，直接返回
            break;
        }
    }
    //长度必须大于等于1，为词
    if(matchFlag < 1 || !flag){
        matchFlag = 0;
    }
    return matchFlag;
}
```
