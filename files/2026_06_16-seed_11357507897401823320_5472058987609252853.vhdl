-- Seed: 11357507897401823320,5472058987609252853

entity lbvxsde is
  port (jam : linkage time; emuujcxb : buffer real; lyjjltdp : linkage boolean; sjulivyq : linkage real);
end lbvxsde;

architecture iikqyo of lbvxsde is
  
begin
  -- Single-driven assignments
  emuujcxb <= 2#1001.0_0_0#;
end iikqyo;

entity jsnxqf is
  port (um : linkage integer; rrzvm : inout real_vector(4 downto 1); rzr : in time; olzyvyo : out real);
end jsnxqf;

architecture luapbiewgy of jsnxqf is
  signal wkaaemuuuk : boolean;
  signal le : real;
  signal sifrhaj : time;
begin
  hyv : entity work.lbvxsde
    port map (jam => sifrhaj, emuujcxb => le, lyjjltdp => wkaaemuuuk, sjulivyq => olzyvyo);
  
  -- Single-driven assignments
  rrzvm <= (16#2_B_8_4.D_B_7#, 033.13230, 3_4_1_3.3, 41.3_4_0);
end luapbiewgy;

library ieee;
use ieee.std_logic_1164.all;

entity xsskwwt is
  port (mbjatcjw : buffer std_logic; tpckz : in integer; p : out boolean; egkbxotjq : out real);
end xsskwwt;

architecture vic of xsskwwt is
  signal zqxtmxjqh : real;
  signal c : time;
begin
  o : entity work.lbvxsde
    port map (jam => c, emuujcxb => egkbxotjq, lyjjltdp => p, sjulivyq => zqxtmxjqh);
  
  -- Multi-driven assignments
  mbjatcjw <= 'X';
  mbjatcjw <= 'Z';
  mbjatcjw <= 'U';
  mbjatcjw <= 'L';
end vic;

library ieee;
use ieee.std_logic_1164.all;

entity ujnxbkvd is
  port (hokxgeda : inout std_logic; s : out std_logic; kagq : linkage integer);
end ujnxbkvd;

architecture vm of ujnxbkvd is
  signal pqbgmw : real;
  signal qmszievsm : boolean;
  signal iazizeb : real;
  signal xwfxlbkq : time;
  signal rfxqrtmjkq : real;
  signal rbki : boolean;
  signal rnyyirm : real;
  signal ycwcab : time;
begin
  dp : entity work.lbvxsde
    port map (jam => ycwcab, emuujcxb => rnyyirm, lyjjltdp => rbki, sjulivyq => rfxqrtmjkq);
  vcaceqr : entity work.lbvxsde
    port map (jam => xwfxlbkq, emuujcxb => iazizeb, lyjjltdp => qmszievsm, sjulivyq => pqbgmw);
  
  -- Multi-driven assignments
  s <= '0';
  s <= 'L';
  s <= 'Z';
  s <= 'W';
end vm;



-- Seed after: 17967609954594838948,5472058987609252853
