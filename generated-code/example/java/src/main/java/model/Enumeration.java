package model;

import util.StreamUtil;

public enum Enumeration {
    VALUE_ONE(0),
    VALUE_TWO(1);
    public int tag;
    Enumeration(int tag) {
      this.tag = tag;
    }
}
