-- Seed: 3895797303769566655,5306691039457971049

entity ecabxpw is
  port (ykxkuiwnsp : buffer boolean; x : linkage time_vector(1 to 0); bkvkqayc : inout boolean);
end ecabxpw;

architecture jtiyje of ecabxpw is
  
begin
  -- Single-driven assignments
  bkvkqayc <= bkvkqayc;
  ykxkuiwnsp <= TRUE;
end jtiyje;

entity pkq is
  port (mxymosjex : linkage severity_level; kcqhlzjwbc : buffer bit; lqtzs : inout real);
end pkq;

architecture cs of pkq is
  signal xkxenl : boolean;
  signal qpxwgwggh : time_vector(1 to 0);
  signal zdnichujk : boolean;
  signal woa : boolean;
  signal zht : time_vector(1 to 0);
  signal nwluo : boolean;
begin
  rn : entity work.ecabxpw
    port map (ykxkuiwnsp => nwluo, x => zht, bkvkqayc => woa);
  ue : entity work.ecabxpw
    port map (ykxkuiwnsp => zdnichujk, x => qpxwgwggh, bkvkqayc => xkxenl);
  
  -- Single-driven assignments
  lqtzs <= 2#0_0_0_1_0.0_1_0_1#;
  kcqhlzjwbc <= '1';
end cs;

entity tapw is
  port (wlg : inout time);
end tapw;

architecture xyw of tapw is
  signal hrqshbohct : boolean;
  signal t : time_vector(1 to 0);
  signal lrxwhdj : boolean;
  signal txzgplpkp : boolean;
  signal yc : time_vector(1 to 0);
  signal ylcj : boolean;
  signal xjkwsngm : boolean;
  signal ykvjzgflm : time_vector(1 to 0);
  signal srhlzvsq : boolean;
begin
  gca : entity work.ecabxpw
    port map (ykxkuiwnsp => srhlzvsq, x => ykvjzgflm, bkvkqayc => xjkwsngm);
  wihwufsicd : entity work.ecabxpw
    port map (ykxkuiwnsp => ylcj, x => yc, bkvkqayc => txzgplpkp);
  ktv : entity work.ecabxpw
    port map (ykxkuiwnsp => lrxwhdj, x => t, bkvkqayc => hrqshbohct);
end xyw;



-- Seed after: 8063335367131824540,5306691039457971049
