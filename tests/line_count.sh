#!/bin/bash
#

pushd "python-reference"
CLOC="../cloc"

echo ""

echo -e "Feature\t\t\t# of tests\tLOC"

echo -en "Built-in Datatypes\t"
echo -n `ls builtin/* bool/* dict/* functions/* lists/* tuple/* types/* | wc -w`
echo -en "\t\t"
echo `$CLOC builtin/ bool/ dict/ functions/ lists/ tuple/ types/ 2>&1  | grep SUM | awk '{print $5}'`

echo -en "Scope\t\t\t"
echo -n `ls scope/* | wc -l`
echo -en "\t\t"
echo `$CLOC scope/ 2>&1  | grep SUM | awk '{print $5}'`

echo -en "Exceptions\t\t"
echo -n `ls exceptions/* | wc -l`
echo -en "\t\t"
echo `$CLOC exceptions/ 2>&1  | grep SUM | awk '{print $5}'`

echo -en "Multiple Inheritance\t"
echo -n `ls multiple-inheritance/* super/* | wc -l`
echo -en "\t\t"
echo `$CLOC multiple-inheritance/ super/ 2>&1  | grep SUM | awk '{print $5}'`

echo -en "Properties\t\t"
echo -n `ls property/* | wc -l`
echo -en "\t\t"
echo `$CLOC property/ 2>&1  | grep SUM | awk '{print $5}'`

echo -en "Iteration\t\t"
echo -n `ls iter/* range/* | wc -l`
echo -en "\t\t"
echo `$CLOC iter/ range/ 2>&1  | grep SUM | awk '{print $5}'`

echo -en "Generators\t\t"
echo -n `ls generators/* | wc -l`
echo -en "\t\t"
echo `$CLOC generators/ 2>&1  | grep SUM | awk '{print $5}'`

echo -en "Modules\t\t\t"
echo -n `ls modules/* | wc -l`
echo -en "\t\t"
echo `$CLOC modules/ 2>&1  | grep SUM | awk '{print $5}'`

echo ""

echo -en "Total\t\t\t"
echo -n `ls */* | wc -l`
echo -en "\t\t"
echo `$CLOC . 2>&1  | grep SUM | awk '{print $5}'`

echo ""

popd
