; ModuleID = 'pcfprog'


 


declare external ccc  i64* @pcf_mkclosure(i64* (i64*, i64*)*, i64, ...)    


declare external ccc  i64 @pcf_print(i64)    


@ans = internal   global i64* zeroinitializer


define external ccc  i64* @pcfmain()    {
pcfmain:
  %__r_1 = inttoptr i64 0 to i64* 
  %__r_2 = inttoptr i64 0 to i64* 
  %__r_3 = ptrtoint i64* %__r_1 to i64 
  %__r_4 = ptrtoint i64* %__r_2 to i64 
  %__r_5 = add   i64 %__r_3, %__r_4 
  %r_1 = inttoptr i64 %__r_5 to i64* 
  %__r_6 = inttoptr i64 0 to i64* 
  %__r_7 = ptrtoint i64* %r_1 to i64 
  %__r_8 = ptrtoint i64* %__r_6 to i64 
  %__r_9 = add   i64 %__r_7, %__r_8 
  %____x20 = inttoptr i64 %__r_9 to i64* 
  %__r_10 = inttoptr i64 1 to i64* 
  %__r_11 = inttoptr i64 0 to i64* 
  %__r_12 = ptrtoint i64* %__r_10 to i64 
  %__r_13 = ptrtoint i64* %__r_11 to i64 
  %__r_14 = add   i64 %__r_12, %__r_13 
  %r_2 = inttoptr i64 %__r_14 to i64* 
  %__r_15 = inttoptr i64 0 to i64* 
  %__r_16 = ptrtoint i64* %r_2 to i64 
  %__r_17 = ptrtoint i64* %__r_15 to i64 
  %__r_18 = add   i64 %__r_16, %__r_17 
  %____x01 = inttoptr i64 %__r_18 to i64* 
  %__r_19 = inttoptr i64 3 to i64* 
  %__r_20 = inttoptr i64 0 to i64* 
  %__r_21 = ptrtoint i64* %__r_19 to i64 
  %__r_22 = ptrtoint i64* %__r_20 to i64 
  %__r_23 = add   i64 %__r_21, %__r_22 
  %r_3 = inttoptr i64 %__r_23 to i64* 
  %__r_24 = inttoptr i64 0 to i64* 
  %__r_25 = ptrtoint i64* %r_3 to i64 
  %__r_26 = ptrtoint i64* %__r_24 to i64 
  %__r_27 = add   i64 %__r_25, %__r_26 
  %____x12 = inttoptr i64 %__r_27 to i64* 
  %__r_28 = ptrtoint i64* %____x01 to i64 
  %__r_29 = ptrtoint i64* %____x12 to i64 
  %__r_30 = add   i64 %__r_28, %__r_29 
  %r_4 = inttoptr i64 %__r_30 to i64* 
  %__r_31 = inttoptr i64 0 to i64* 
  %__r_32 = ptrtoint i64* %r_4 to i64 
  %__r_33 = ptrtoint i64* %__r_31 to i64 
  %__r_34 = add   i64 %__r_32, %__r_33 
  %____x33 = inttoptr i64 %__r_34 to i64* 
  %__r_35 = ptrtoint i64* %____x20 to i64 
  %__r_36 = ptrtoint i64* %____x33 to i64 
  %__r_37 = add   i64 %__r_35, %__r_36 
  %r_5 = inttoptr i64 %__r_37 to i64* 
  %__r_38 = ptrtoint i64* %r_5 to i64 
  %__r_39 =  call ccc  i64  @pcf_print(i64  %__r_38)  
  %r_0 = inttoptr i64 %__r_39 to i64* 
  %__r_40 = inttoptr i64 0 to i64* 
  %__r_41 = ptrtoint i64* %r_0 to i64 
  %__r_42 = ptrtoint i64* %__r_40 to i64 
  %__r_43 = add   i64 %__r_41, %__r_42 
  %__r_44 = inttoptr i64 %__r_43 to i64* 
  store  i64* %__r_44, i64** @ans 
  ret i64* %r_5 
}