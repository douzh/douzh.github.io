
# 括号匹配

```
算法题：给定一个只包括 '('，')'，'{'，'}'，'['，']' 的字符串，判断字符串是否有效。
有效字符串需满足：
    左括号必须用相同类型的右括号闭合。
    左括号必须以正确的顺序闭合。

举例：字符串 "()"有效、"()[]{}"有效、"(]"无效、"([)]"无效、"{[]}"有效。

```

```text
class Solution {
    public boolean isValid(String s) {
      Stack<Character> stack = new Stack<Character>();
      HashMap<Character,Character> map = new HashMap<Character,Character>();
      map.put('(', ')');
      map.put('{','}' );
      map.put('[', ']');

      for(int i=0;i<s.length();i++){
        char c = s.charAt(i);
        if(map.containsKey(c)){
          stack.push(c);
        }else{
          if(stack.isEmpty()) return false;
          char temp = stack.pop();
          if(map.get(temp)!=c) return false;
        }
      } 
      return stack.isEmpty();
    }
}
```
