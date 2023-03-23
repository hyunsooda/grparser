package anotherpackage

import (
	"fmt"

	"example/mypack"
)

type ABC struct{}

func (abc ABC) Hi() string {
	abcabc := mypack.ABC{}
	abcabc.Hi()
	sum := mypack.GetSum()
	fmt.Println(sum)
	return "hello"
}

func (abc *ABC) Hi2() string {
	return "hello"
}

func Myfunc() string {
	defer func() {
		fmt.Println("DEFER!")
	}()
	return "hello"
}
