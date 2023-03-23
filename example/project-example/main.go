package main

import (
	"fmt"

	"example/anotherpackage"
	"example/mypack"
)

type A struct{}

func (a *A) foo(v1, v2, v3 int, s string) int {
	return v1 + v1
}

func (a A) baz(v1, v2 int) {
	return
}

func (a A) bar(v1, v2 int, abc mypack.ABC) int {
	return 1
}

func bar(v1, v2, v3 int, s string) (error, error, error) {
	return nil, nil, nil
}

func foo() (myerr1, myerr2, myerr3 error) {
	myerr1, myerr2, myerr3 = nil, nil, nil
	return
}

func interfaceFunc(a interface{}) {
	v := a.(int)
	fmt.Println(v)
	switch a.(type) {
	case int:
		fmt.Println("int!")
	case float32:
		fmt.Println("float!")
	case string:
		fmt.Println("string!")
	}
}

func main() {
	abc := anotherpackage.ABC{}
	abc2 := &anotherpackage.ABC{}
	abc.Hi()
	abc2.Hi2()

	abcabc := mypack.ABC{}
	abcabc2 := &mypack.ABC{}
	abcabc.Hi()
	abcabc2.Hi()

	for myidx := 0; myidx < 5; myidx++ {
		fmt.Println("HELLO")
		if myidx == 2 {
			fmt.Println("myidx == 2")
		}
	}
	e := 1 + 2 + 3 + 4 + 5 + 6 + 7 + 8 + 9 + 10
	e += 1
	_, b, c := foo()
	mystruct := &A{}
	ptr1 := &mystruct
	ptr2 := &ptr1
	ptr3 := &ptr2
	mystruct2 := A{}
	ret := mystruct.foo(1, 2, 3, "hello")
	fmt.Println(b, c, e, ret, mystruct2.bar(1, 2, abcabc))
	d := ***ptr3
	r1, r2, r3 := bar(1, 2, 3, "hello")
	mystruct2.baz(1, 2)
	fmt.Println(d, r1, r2, r3)

	litarr := []int{1, 2, 3}
	var arr2 []int
	arr := []int{1, 2, 3, 4, 5}
	arr[3] = 123
	arr2 = append(arr2, 1)
	idx := 3
	if idx == 1 {
		fmt.Println(arr, arr[idx])
	} else {
		fmt.Println(arr, arr[idx])
	}
	ch := make(chan bool, arr[3])
	ch <- true
	<-ch
	var k interface{}
	var kk any
	k = 1234
	kk = 1235
	arr3 := arr[idx-1 : idx+1]
	fmt.Println(k, kk, arr3, arr[idx-1:idx+1])
	nArr := [5][2]int{{0, 0}, {1, 2}, {2, 4}, {3, 6}, {4, 8}}
	fmt.Println(nArr, litarr)
	anotherpackage.Myfunc()
	interfaceFunc(3.14)
	err := fmt.Errorf("my error")
	kk = err

	var anySlice []any
	anySlice[0] = 123

	var anyArr [3]any
	anyArr[0] = 123

	a := func() int {
		return 123
	}
	_ = a()
}
