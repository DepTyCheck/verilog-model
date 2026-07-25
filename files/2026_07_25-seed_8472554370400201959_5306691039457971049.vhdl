-- Seed: 8472554370400201959,5306691039457971049

library ieee;
use ieee.std_logic_1164.all;

entity dgo is
  port (aotrfeni : inout time; tpw : buffer std_logic; ljriwbw : buffer std_logic; nllqkpg : in time_vector(3 downto 0));
end dgo;

architecture x of dgo is
  
begin
  -- Multi-driven assignments
  ljriwbw <= ljriwbw;
end x;

library ieee;
use ieee.std_logic_1164.all;

entity ylylcgzpk is
  port (jyndwofl : linkage std_logic_vector(3 to 1); bbhgrywmz : buffer time);
end ylylcgzpk;

library ieee;
use ieee.std_logic_1164.all;

architecture tnoc of ylylcgzpk is
  signal igfnyk : std_logic;
  signal dltqekir : time_vector(3 downto 0);
  signal vjnnobmjb : std_logic;
  signal ijhs : time;
begin
  mtmn : entity work.dgo
    port map (aotrfeni => ijhs, tpw => vjnnobmjb, ljriwbw => vjnnobmjb, nllqkpg => dltqekir);
  hlaohxnep : entity work.dgo
    port map (aotrfeni => bbhgrywmz, tpw => vjnnobmjb, ljriwbw => igfnyk, nllqkpg => dltqekir);
  
  -- Single-driven assignments
  dltqekir <= dltqekir;
  
  -- Multi-driven assignments
  vjnnobmjb <= '1';
  vjnnobmjb <= 'X';
end tnoc;

entity mmvckmd is
  port (lxnylufokl : out integer);
end mmvckmd;

library ieee;
use ieee.std_logic_1164.all;

architecture tswvmefif of mmvckmd is
  signal hanjuym : time_vector(3 downto 0);
  signal zltcubvqj : std_logic;
  signal xdzevmdzxd : std_logic;
  signal fplth : time;
  signal nlwhiyj : time;
  signal lsrwto : std_logic_vector(3 to 1);
begin
  cvzq : entity work.ylylcgzpk
    port map (jyndwofl => lsrwto, bbhgrywmz => nlwhiyj);
  igxntk : entity work.dgo
    port map (aotrfeni => fplth, tpw => xdzevmdzxd, ljriwbw => zltcubvqj, nllqkpg => hanjuym);
  
  -- Single-driven assignments
  lxnylufokl <= 8#3_7_0_6#;
  hanjuym <= hanjuym;
  
  -- Multi-driven assignments
  lsrwto <= lsrwto;
end tswvmefif;

entity n is
  port (zla : linkage time);
end n;

library ieee;
use ieee.std_logic_1164.all;

architecture jzzjvxr of n is
  signal zbuxt : integer;
  signal pgttxmvg : integer;
  signal gmwffirxzs : integer;
  signal bmkxtge : time_vector(3 downto 0);
  signal tl : std_logic;
  signal sj : time;
begin
  cjhbcg : entity work.dgo
    port map (aotrfeni => sj, tpw => tl, ljriwbw => tl, nllqkpg => bmkxtge);
  osnuwe : entity work.mmvckmd
    port map (lxnylufokl => gmwffirxzs);
  xtrxmq : entity work.mmvckmd
    port map (lxnylufokl => pgttxmvg);
  kesql : entity work.mmvckmd
    port map (lxnylufokl => zbuxt);
  
  -- Multi-driven assignments
  tl <= 'Z';
  tl <= 'L';
  tl <= tl;
end jzzjvxr;



-- Seed after: 14796471782150762911,5306691039457971049
