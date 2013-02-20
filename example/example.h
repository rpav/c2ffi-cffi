#define FOO (1 << 2)

#define QUUX "abc"

const int BAR = FOO + 10;

extern SomeExtern;

void blah(char *x[]);

extern char *foo;

typedef struct my_point {
  int x;
  int y;
  int odd_value[BAR + 1];
} my_point_t;

typedef struct {
  int a, b;
} anonymous_t;

union my_union {
  char c;
  int i;
  double d;
};

enum some_values {
  a_value,
  another_value,
  yet_another_value
};

void do_something(my_point_t *p, int x, int y);

