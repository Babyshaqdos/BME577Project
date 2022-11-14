##ANOVA scripting

oneway.test(dataset$category ~ dataset$Group) #Group (phenotype)

#post hoc highly significant difference test
TukeyHSD(aov(dataset$category ~ dataset$Group)) #aov() fit analysis of variance count


#This version of anova assumes equal variance among groups
anova(lm(dataset$category ~ dataset$Group))


#Two way ANOVA
anova(lm(dataset$dependentVar ~ dataset$independentVar + dataset$independentVar2))

