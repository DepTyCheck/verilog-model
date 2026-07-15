-- Seed: 10572913333693687881,2983771601630957889

use std.reflection.all;

entity s is
  port ( yptbiowffx : linkage integer
  ; variable asvfrslxta : inout array_subtype_mirror_pt
  ; vattzteuoh : in boolean_vector(0 downto 2)
  ; dhceoiz : inout real
  );
end s;

architecture vzjjqxkj of s is
  
begin
  -- Single-driven assignments
  dhceoiz <= dhceoiz;
end vzjjqxkj;

use std.reflection.all;

entity lo is
  port (variable ykyxojg : inout access_subtype_mirror_pt; jrxbqkaova : inout real);
end lo;

use std.reflection.all;

architecture ucrdxz of lo is
  signal x : boolean_vector(0 downto 2);
  shared variable whdyxygte : array_subtype_mirror_pt;
  signal ivqba : integer;
  signal lq : real;
  signal osobzq : boolean_vector(0 downto 2);
  shared variable wvx : array_subtype_mirror_pt;
  signal lkdfm : integer;
  signal bdmk : real;
  shared variable tyhuvccjid : array_subtype_mirror_pt;
  signal hjqmhnle : integer;
  signal lh : real;
  signal vewupvehi : boolean_vector(0 downto 2);
  shared variable glmalr : array_subtype_mirror_pt;
  signal gip : integer;
begin
  bxpenq : entity work.s
    port map (yptbiowffx => gip, asvfrslxta => glmalr, vattzteuoh => vewupvehi, dhceoiz => lh);
  dvptmofm : entity work.s
    port map (yptbiowffx => hjqmhnle, asvfrslxta => tyhuvccjid, vattzteuoh => vewupvehi, dhceoiz => bdmk);
  p : entity work.s
    port map (yptbiowffx => lkdfm, asvfrslxta => wvx, vattzteuoh => osobzq, dhceoiz => lq);
  hasbmuajq : entity work.s
    port map (yptbiowffx => ivqba, asvfrslxta => whdyxygte, vattzteuoh => x, dhceoiz => jrxbqkaova);
  
  -- Single-driven assignments
  vewupvehi <= (others => TRUE);
  x <= (others => TRUE);
end ucrdxz;

use std.reflection.all;

entity hujtww is
  port ( bbmjqph : inout boolean
  ; dolsjs : inout integer
  ; variable jelie : inout file_value_mirror_pt
  ; variable fwzhvspa : inout floating_value_mirror_pt
  );
end hujtww;

use std.reflection.all;

architecture p of hujtww is
  signal mr : real;
  shared variable kjnnyq : array_subtype_mirror_pt;
  signal cryuhlw : integer;
  signal eujgbsebb : real;
  signal szhmon : boolean_vector(0 downto 2);
  shared variable ts : array_subtype_mirror_pt;
  signal fl : real;
  signal gyqcnmg : boolean_vector(0 downto 2);
  shared variable uyvqgja : array_subtype_mirror_pt;
  signal nxhcsezway : integer;
begin
  dotgjb : entity work.s
    port map (yptbiowffx => nxhcsezway, asvfrslxta => uyvqgja, vattzteuoh => gyqcnmg, dhceoiz => fl);
  rtevh : entity work.s
    port map (yptbiowffx => dolsjs, asvfrslxta => ts, vattzteuoh => szhmon, dhceoiz => eujgbsebb);
  nrpos : entity work.s
    port map (yptbiowffx => cryuhlw, asvfrslxta => kjnnyq, vattzteuoh => szhmon, dhceoiz => mr);
  
  -- Single-driven assignments
  bbmjqph <= FALSE;
  gyqcnmg <= (others => TRUE);
  szhmon <= gyqcnmg;
end p;



-- Seed after: 5809934092608497449,2983771601630957889
