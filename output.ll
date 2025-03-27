; ModuleID = 'pcfprog'


 


declare external ccc  i64* @pcf_mkclosure(i64* (i64*, i64*)*, i64, ...)    


declare external ccc  i64 @pcf_print(i64)    


define external ccc  i64* @__4(i64*  %__clos5, i64*  %__y3)    {
__4:
  %__r_2 = bitcast i64* %__clos5 to i64** 
  %addr1 = getelementptr  i64*, i64** %__r_2, i64 1 
  %r_0 = load  i64*, i64** %addr1 
  %__r_3 = inttoptr i64 0 to i64* 
  %__r_4 = ptrtoint i64* %r_0 to i64 
  %__r_5 = ptrtoint i64* %__r_3 to i64 
  %__r_6 = add   i64 %__r_4, %__r_5 
  %__x0 = inttoptr i64 %__r_6 to i64* 
  %__r_7 = ptrtoint i64* %__x0 to i64 
  %__r_8 = ptrtoint i64* %__y3 to i64 
  %__r_9 = add   i64 %__r_7, %__r_8 
  %r_1 = inttoptr i64 %__r_9 to i64* 
  ret i64* %r_1 
}


define external ccc  i64* @__1(i64*  %__clos2, i64*  %__x0)    {
__1:
  %r_2 =  call ccc  i64* (i64* (i64*, i64*)*, i64, ...) @pcf_mkclosure(i64* (i64*, i64*)*  @__4, i64  1, i64*  %__x0)  
  ret i64* %r_2 
}


define external ccc  i64* @__10(i64*  %__clos11, i64*  %__y9)    {
__10:
  %__r_2 = bitcast i64* %__clos11 to i64** 
  %addr1 = getelementptr  i64*, i64** %__r_2, i64 1 
  %r_3 = load  i64*, i64** %addr1 
  %__r_3 = inttoptr i64 0 to i64* 
  %__r_4 = ptrtoint i64* %r_3 to i64 
  %__r_5 = ptrtoint i64* %__r_3 to i64 
  %__r_6 = add   i64 %__r_4, %__r_5 
  %__x6 = inttoptr i64 %__r_6 to i64* 
  %__r_7 = ptrtoint i64* %__x6 to i64 
  %__r_8 = ptrtoint i64* %__y9 to i64 
  %__r_9 = sub   i64 %__r_7, %__r_8 
  %__r_10 = icmp slt i64 0, %__r_9 
  %__r_11 = zext i1 %__r_10 to i64  
  %__r_12 = mul   i64 %__r_9, %__r_11 
  %r_4 = inttoptr i64 %__r_12 to i64* 
  ret i64* %r_4 
}


define external ccc  i64* @__7(i64*  %__clos8, i64*  %__x6)    {
__7:
  %r_5 =  call ccc  i64* (i64* (i64*, i64*)*, i64, ...) @pcf_mkclosure(i64* (i64*, i64*)*  @__10, i64  1, i64*  %__x6)  
  ret i64* %r_5 
}


@z = internal   global i64* zeroinitializer


@suma = internal   global i64* zeroinitializer


@resta = internal   global i64* zeroinitializer


@ans = internal   global i64* zeroinitializer


define external ccc  i64* @pcfmain()    {
pcfmain:
  %__r_1 = inttoptr i64 4 to i64* 
  %__r_2 = inttoptr i64 0 to i64* 
  %__r_3 = ptrtoint i64* %__r_1 to i64 
  %__r_4 = ptrtoint i64* %__r_2 to i64 
  %__r_5 = add   i64 %__r_3, %__r_4 
  %r_6 = inttoptr i64 %__r_5 to i64* 
  %__r_6 = inttoptr i64 0 to i64* 
  %__r_7 = ptrtoint i64* %r_6 to i64 
  %__r_8 = ptrtoint i64* %__r_6 to i64 
  %__r_9 = add   i64 %__r_7, %__r_8 
  %__r_10 = inttoptr i64 %__r_9 to i64* 
  store  i64* %__r_10, i64** @z 
  %r_7 =  call ccc  i64* (i64* (i64*, i64*)*, i64, ...) @pcf_mkclosure(i64* (i64*, i64*)*  @__1, i64  0)  
  %__r_11 = inttoptr i64 0 to i64* 
  %__r_12 = ptrtoint i64* %r_7 to i64 
  %__r_13 = ptrtoint i64* %__r_11 to i64 
  %__r_14 = add   i64 %__r_12, %__r_13 
  %__r_15 = inttoptr i64 %__r_14 to i64* 
  store  i64* %__r_15, i64** @suma 
  %r_8 =  call ccc  i64* (i64* (i64*, i64*)*, i64, ...) @pcf_mkclosure(i64* (i64*, i64*)*  @__7, i64  0)  
  %__r_16 = inttoptr i64 0 to i64* 
  %__r_17 = ptrtoint i64* %r_8 to i64 
  %__r_18 = ptrtoint i64* %__r_16 to i64 
  %__r_19 = add   i64 %__r_17, %__r_18 
  %__r_20 = inttoptr i64 %__r_19 to i64* 
  store  i64* %__r_20, i64** @resta 
  %__r_21 = load  i64*, i64** @suma 
  %__r_22 = inttoptr i64 0 to i64* 
  %__r_23 = ptrtoint i64* %__r_21 to i64 
  %__r_24 = ptrtoint i64* %__r_22 to i64 
  %__r_25 = add   i64 %__r_23, %__r_24 
  %__a14 = inttoptr i64 %__r_25 to i64* 
  %__r_27 = bitcast i64* %__a14 to i64** 
  %addr26 = getelementptr  i64*, i64** %__r_27, i64 0 
  %r_11 = load  i64*, i64** %addr26 
  %__r_28 = load  i64*, i64** @resta 
  %__r_29 = inttoptr i64 0 to i64* 
  %__r_30 = ptrtoint i64* %__r_28 to i64 
  %__r_31 = ptrtoint i64* %__r_29 to i64 
  %__r_32 = add   i64 %__r_30, %__r_31 
  %__a12 = inttoptr i64 %__r_32 to i64* 
  %__r_34 = bitcast i64* %__a12 to i64** 
  %addr33 = getelementptr  i64*, i64** %__r_34, i64 0 
  %r_13 = load  i64*, i64** %addr33 
  %fun35 = bitcast i64* %r_13 to i64* (i64*, i64*)* 
  %__r_36 = load  i64*, i64** @z 
  %r_12 =  call ccc  i64*  %fun35(i64*  %__a12, i64*  %__r_36)  
  %__r_37 = inttoptr i64 0 to i64* 
  %__r_38 = ptrtoint i64* %r_12 to i64 
  %__r_39 = ptrtoint i64* %__r_37 to i64 
  %__r_40 = add   i64 %__r_38, %__r_39 
  %__a13 = inttoptr i64 %__r_40 to i64* 
  %__r_42 = bitcast i64* %__a13 to i64** 
  %addr41 = getelementptr  i64*, i64** %__r_42, i64 0 
  %r_15 = load  i64*, i64** %addr41 
  %fun43 = bitcast i64* %r_15 to i64* (i64*, i64*)* 
  %__r_44 = load  i64*, i64** @z 
  %r_14 =  call ccc  i64*  %fun43(i64*  %__a13, i64*  %__r_44)  
  %fun45 = bitcast i64* %r_11 to i64* (i64*, i64*)* 
  %r_10 =  call ccc  i64*  %fun45(i64*  %__a14, i64*  %r_14)  
  %__r_46 = inttoptr i64 0 to i64* 
  %__r_47 = ptrtoint i64* %r_10 to i64 
  %__r_48 = ptrtoint i64* %__r_46 to i64 
  %__r_49 = add   i64 %__r_47, %__r_48 
  %__a21 = inttoptr i64 %__r_49 to i64* 
  %__r_51 = bitcast i64* %__a21 to i64** 
  %addr50 = getelementptr  i64*, i64** %__r_51, i64 0 
  %r_17 = load  i64*, i64** %addr50 
  %__r_52 = load  i64*, i64** @suma 
  %__r_53 = inttoptr i64 0 to i64* 
  %__r_54 = ptrtoint i64* %__r_52 to i64 
  %__r_55 = ptrtoint i64* %__r_53 to i64 
  %__r_56 = add   i64 %__r_54, %__r_55 
  %__a17 = inttoptr i64 %__r_56 to i64* 
  %__r_58 = bitcast i64* %__a17 to i64** 
  %addr57 = getelementptr  i64*, i64** %__r_58, i64 0 
  %r_19 = load  i64*, i64** %addr57 
  %__r_59 = load  i64*, i64** @resta 
  %__r_60 = inttoptr i64 0 to i64* 
  %__r_61 = ptrtoint i64* %__r_59 to i64 
  %__r_62 = ptrtoint i64* %__r_60 to i64 
  %__r_63 = add   i64 %__r_61, %__r_62 
  %__a15 = inttoptr i64 %__r_63 to i64* 
  %__r_65 = bitcast i64* %__a15 to i64** 
  %addr64 = getelementptr  i64*, i64** %__r_65, i64 0 
  %r_21 = load  i64*, i64** %addr64 
  %fun66 = bitcast i64* %r_21 to i64* (i64*, i64*)* 
  %__r_67 = load  i64*, i64** @z 
  %r_20 =  call ccc  i64*  %fun66(i64*  %__a15, i64*  %__r_67)  
  %__r_68 = inttoptr i64 0 to i64* 
  %__r_69 = ptrtoint i64* %r_20 to i64 
  %__r_70 = ptrtoint i64* %__r_68 to i64 
  %__r_71 = add   i64 %__r_69, %__r_70 
  %__a16 = inttoptr i64 %__r_71 to i64* 
  %__r_73 = bitcast i64* %__a16 to i64** 
  %addr72 = getelementptr  i64*, i64** %__r_73, i64 0 
  %r_23 = load  i64*, i64** %addr72 
  %__r_74 = inttoptr i64 3 to i64* 
  %__r_75 = inttoptr i64 0 to i64* 
  %__r_76 = ptrtoint i64* %__r_74 to i64 
  %__r_77 = ptrtoint i64* %__r_75 to i64 
  %__r_78 = add   i64 %__r_76, %__r_77 
  %r_24 = inttoptr i64 %__r_78 to i64* 
  %fun79 = bitcast i64* %r_23 to i64* (i64*, i64*)* 
  %r_22 =  call ccc  i64*  %fun79(i64*  %__a16, i64*  %r_24)  
  %fun80 = bitcast i64* %r_19 to i64* (i64*, i64*)* 
  %r_18 =  call ccc  i64*  %fun80(i64*  %__a17, i64*  %r_22)  
  %__r_81 = inttoptr i64 0 to i64* 
  %__r_82 = ptrtoint i64* %r_18 to i64 
  %__r_83 = ptrtoint i64* %__r_81 to i64 
  %__r_84 = add   i64 %__r_82, %__r_83 
  %__a20 = inttoptr i64 %__r_84 to i64* 
  %__r_86 = bitcast i64* %__a20 to i64** 
  %addr85 = getelementptr  i64*, i64** %__r_86, i64 0 
  %r_26 = load  i64*, i64** %addr85 
  %__r_87 = load  i64*, i64** @suma 
  %__r_88 = inttoptr i64 0 to i64* 
  %__r_89 = ptrtoint i64* %__r_87 to i64 
  %__r_90 = ptrtoint i64* %__r_88 to i64 
  %__r_91 = add   i64 %__r_89, %__r_90 
  %__a18 = inttoptr i64 %__r_91 to i64* 
  %__r_93 = bitcast i64* %__a18 to i64** 
  %addr92 = getelementptr  i64*, i64** %__r_93, i64 0 
  %r_28 = load  i64*, i64** %addr92 
  %__r_94 = inttoptr i64 1 to i64* 
  %__r_95 = inttoptr i64 0 to i64* 
  %__r_96 = ptrtoint i64* %__r_94 to i64 
  %__r_97 = ptrtoint i64* %__r_95 to i64 
  %__r_98 = add   i64 %__r_96, %__r_97 
  %r_29 = inttoptr i64 %__r_98 to i64* 
  %fun99 = bitcast i64* %r_28 to i64* (i64*, i64*)* 
  %r_27 =  call ccc  i64*  %fun99(i64*  %__a18, i64*  %r_29)  
  %__r_100 = inttoptr i64 0 to i64* 
  %__r_101 = ptrtoint i64* %r_27 to i64 
  %__r_102 = ptrtoint i64* %__r_100 to i64 
  %__r_103 = add   i64 %__r_101, %__r_102 
  %__a19 = inttoptr i64 %__r_103 to i64* 
  %__r_105 = bitcast i64* %__a19 to i64** 
  %addr104 = getelementptr  i64*, i64** %__r_105, i64 0 
  %r_31 = load  i64*, i64** %addr104 
  %__r_106 = inttoptr i64 2 to i64* 
  %__r_107 = inttoptr i64 0 to i64* 
  %__r_108 = ptrtoint i64* %__r_106 to i64 
  %__r_109 = ptrtoint i64* %__r_107 to i64 
  %__r_110 = add   i64 %__r_108, %__r_109 
  %r_32 = inttoptr i64 %__r_110 to i64* 
  %fun111 = bitcast i64* %r_31 to i64* (i64*, i64*)* 
  %r_30 =  call ccc  i64*  %fun111(i64*  %__a19, i64*  %r_32)  
  %fun112 = bitcast i64* %r_26 to i64* (i64*, i64*)* 
  %r_25 =  call ccc  i64*  %fun112(i64*  %__a20, i64*  %r_30)  
  %fun113 = bitcast i64* %r_17 to i64* (i64*, i64*)* 
  %r_16 =  call ccc  i64*  %fun113(i64*  %__a21, i64*  %r_25)  
  %__r_114 = ptrtoint i64* %r_16 to i64 
  %__r_115 =  call ccc  i64  @pcf_print(i64  %__r_114)  
  %r_9 = inttoptr i64 %__r_115 to i64* 
  %__r_116 = inttoptr i64 0 to i64* 
  %__r_117 = ptrtoint i64* %r_9 to i64 
  %__r_118 = ptrtoint i64* %__r_116 to i64 
  %__r_119 = add   i64 %__r_117, %__r_118 
  %__r_120 = inttoptr i64 %__r_119 to i64* 
  store  i64* %__r_120, i64** @ans 
  ret i64* %r_16 
}