-- Seed: 12526979334329447108,4122021602305298647

entity qwzw is
  port (kycgbveaw : linkage bit; lwplhff : in time);
end qwzw;

architecture gqi of qwzw is
  
begin
  
end gqi;

entity wsw is
  port (kdjjktxn : inout real; ykxkjbbumk : inout integer);
end wsw;

architecture bbkkfoart of wsw is
  signal rvpmr : time;
  signal ufldzwf : bit;
  signal kcqioh : bit;
  signal x : bit;
  signal amjbbbo : time;
  signal mufm : bit;
begin
  yf : entity work.qwzw
    port map (kycgbveaw => mufm, lwplhff => amjbbbo);
  liasvyzxss : entity work.qwzw
    port map (kycgbveaw => x, lwplhff => amjbbbo);
  hhkwgrf : entity work.qwzw
    port map (kycgbveaw => kcqioh, lwplhff => amjbbbo);
  tt : entity work.qwzw
    port map (kycgbveaw => ufldzwf, lwplhff => rvpmr);
end bbkkfoart;

entity cfzcdwfrq is
  port (mxwuxv : out time);
end cfzcdwfrq;

architecture lflrzida of cfzcdwfrq is
  signal y : time;
  signal md : bit;
begin
  lo : entity work.qwzw
    port map (kycgbveaw => md, lwplhff => y);
  
  -- Single-driven assignments
  y <= mxwuxv;
  mxwuxv <= 2#11# us;
end lflrzida;

entity ibkfp is
  port (ngv : buffer time);
end ibkfp;

architecture ubggp of ibkfp is
  signal gbdayhivjj : time;
  signal gzivt : bit;
begin
  uqny : entity work.cfzcdwfrq
    port map (mxwuxv => ngv);
  wdiogpnkgx : entity work.qwzw
    port map (kycgbveaw => gzivt, lwplhff => gbdayhivjj);
  
  -- Single-driven assignments
  gbdayhivjj <= ngv;
end ubggp;



-- Seed after: 16989278894727503238,4122021602305298647
