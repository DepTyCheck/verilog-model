-- Seed: 17518096052554841775,11127274767545411571

entity a is
  port (q : buffer bit; njyzvor : in integer; i : linkage real; tuowcefn : inout time);
end a;

architecture n of a is
  
begin
  -- Single-driven assignments
  tuowcefn <= 16#5_F# us;
  q <= '1';
end n;

entity hjbvmcfksd is
  port (yyeo : in time; dnum : out real; imzojw : buffer severity_level; cxwjcdrner : linkage severity_level);
end hjbvmcfksd;

architecture xjwxx of hjbvmcfksd is
  signal maw : time;
  signal hufmvxr : real;
  signal vawxmgth : integer;
  signal embovwz : bit;
begin
  k : entity work.a
    port map (q => embovwz, njyzvor => vawxmgth, i => hufmvxr, tuowcefn => maw);
  
  -- Single-driven assignments
  imzojw <= FAILURE;
  vawxmgth <= 2#100#;
  dnum <= hufmvxr;
end xjwxx;

entity orbvaq is
  port (pvmyczwfy : inout real; hlsttlh : linkage time);
end orbvaq;

architecture abuny of orbvaq is
  signal uppir : severity_level;
  signal prlxjuqav : severity_level;
  signal xokvd : real;
  signal j : time;
begin
  qyfiseondm : entity work.hjbvmcfksd
    port map (yyeo => j, dnum => xokvd, imzojw => prlxjuqav, cxwjcdrner => uppir);
  
  -- Single-driven assignments
  pvmyczwfy <= xokvd;
  j <= 130 fs;
end abuny;



-- Seed after: 15779891802257237615,11127274767545411571
