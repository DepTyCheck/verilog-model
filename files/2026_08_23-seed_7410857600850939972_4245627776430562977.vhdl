-- Seed: 7410857600850939972,4245627776430562977

library ieee;
use ieee.std_logic_1164.all;

entity ibyieqq is
  port (hyujgqqbfj : buffer integer; fsmvj : in time; xdkxx : linkage integer; wstvce : in std_logic);
end ibyieqq;

architecture zfnhtzaf of ibyieqq is
  
begin
  -- Single-driven assignments
  hyujgqqbfj <= 2#10#;
end zfnhtzaf;

entity uovfm is
  port (so : buffer real; foxrhswkr : out integer; ngemnmkaid : linkage integer);
end uovfm;

library ieee;
use ieee.std_logic_1164.all;

architecture ympcyfzm of uovfm is
  signal ijx : std_logic;
  signal amsgykjol : integer;
  signal ggjwreayhl : time;
  signal iamgwax : integer;
  signal xyo : std_logic;
  signal n : integer;
  signal vuoaqywofz : time;
  signal ywvnnfpci : integer;
begin
  qaxaky : entity work.ibyieqq
    port map (hyujgqqbfj => ywvnnfpci, fsmvj => vuoaqywofz, xdkxx => n, wstvce => xyo);
  juwvioqyt : entity work.ibyieqq
    port map (hyujgqqbfj => iamgwax, fsmvj => ggjwreayhl, xdkxx => amsgykjol, wstvce => xyo);
  svb : entity work.ibyieqq
    port map (hyujgqqbfj => foxrhswkr, fsmvj => vuoaqywofz, xdkxx => ngemnmkaid, wstvce => ijx);
  
  -- Single-driven assignments
  so <= 16#0_E_4_5.8_2#;
  vuoaqywofz <= 2 min;
  ggjwreayhl <= vuoaqywofz;
  
  -- Multi-driven assignments
  xyo <= 'U';
  xyo <= 'L';
  xyo <= ijx;
  xyo <= xyo;
end ympcyfzm;

entity pwhskdqg is
  port (up : inout boolean_vector(0 to 4); nxwrzdtywu : linkage time);
end pwhskdqg;

library ieee;
use ieee.std_logic_1164.all;

architecture axonjvdz of pwhskdqg is
  signal rrdwjcm : std_logic;
  signal uy : integer;
  signal qszckufmrh : time;
  signal nhczx : integer;
  signal fsdqav : integer;
  signal uaikrpmea : integer;
  signal pyzkipj : real;
  signal ucfxr : std_logic;
  signal jrswvd : integer;
  signal matxdjzyjn : time;
  signal bneqbzszt : integer;
begin
  ynsberop : entity work.ibyieqq
    port map (hyujgqqbfj => bneqbzszt, fsmvj => matxdjzyjn, xdkxx => jrswvd, wstvce => ucfxr);
  xt : entity work.uovfm
    port map (so => pyzkipj, foxrhswkr => uaikrpmea, ngemnmkaid => fsdqav);
  pwyfev : entity work.ibyieqq
    port map (hyujgqqbfj => nhczx, fsmvj => qszckufmrh, xdkxx => uy, wstvce => rrdwjcm);
  
  -- Single-driven assignments
  qszckufmrh <= matxdjzyjn;
  matxdjzyjn <= matxdjzyjn;
  up <= (TRUE, FALSE, FALSE, TRUE, FALSE);
end axonjvdz;



-- Seed after: 9598178076264605516,4245627776430562977
