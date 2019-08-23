## Introduction

[![Build Status](https://travis-ci.org/YouerSu/ParGer.svg?branch=master)](https://travis-ci.org/YouerSu/ParGer)

ParGer is a ParserGen which is bases on Haskell,It can save your time if you don't want to write a parser by yourself(It's very boring).

## How to use ParGer

ParGer has a DSL for generating Parser,It's very similar to EBNF's syntax,so,you don't need to worry about it.

- [Exp] : one or zero Exp
- {Exp} : many or zero Exp
- Exp|Exp : Exp or Exp
- Exp + Exp : Exp and Exp connection
- 'Exp : Exp is a specific symbol
- name ::= Exp : Define a Rule

To write a ParGer File,you need to add ";" after each statement.  
Besides,don't forget to set up the lang of Parser at the beginning

    lang ::= name

> Suppose Language:

- haskell
