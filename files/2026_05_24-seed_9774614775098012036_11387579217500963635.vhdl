-- Seed: 9774614775098012036,11387579217500963635

library ieee;
use ieee.std_logic_1164.all;

entity vbtj is
  port (jakkjcr : out std_logic; msvqdb : out integer; atxi : linkage integer; xiz : linkage std_logic_vector(4 downto 2));
end vbtj;



architecture qizzhz of vbtj is
  
begin
  
end qizzhz;



entity q is
  port (gvbeepxo : in bit);
end q;

library ieee;
use ieee.std_logic_1164.all;

architecture ygdjdlsz of q is
  signal foab : std_logic_vector(4 downto 2);
  signal qoatrnpxqd : integer;
  signal hky : std_logic_vector(4 downto 2);
  signal intz : std_logic;
  signal j : std_logic_vector(4 downto 2);
  signal nhvmgjyiq : integer;
  signal spjg : integer;
  signal vfocr : std_logic_vector(4 downto 2);
  signal om : integer;
  signal zxfgsumgx : integer;
  signal ppho : std_logic;
begin
  cztbaiq : entity work.vbtj
    port map (jakkjcr => ppho, msvqdb => zxfgsumgx, atxi => om, xiz => vfocr);
  clcdfyqz : entity work.vbtj
    port map (jakkjcr => ppho, msvqdb => spjg, atxi => nhvmgjyiq, xiz => j);
  so : entity work.vbtj
    port map (jakkjcr => intz, msvqdb => om, atxi => zxfgsumgx, xiz => hky);
  bmiakt : entity work.vbtj
    port map (jakkjcr => intz, msvqdb => qoatrnpxqd, atxi => spjg, xiz => foab);
end ygdjdlsz;



entity jgr is
  port (wnc : out boolean_vector(2 downto 2); olcurz : linkage integer; eya : inout character);
end jgr;

library ieee;
use ieee.std_logic_1164.all;

architecture wu of jgr is
  signal er : bit;
  signal k : bit;
  signal tqw : std_logic_vector(4 downto 2);
  signal xurnzsxkr : integer;
  signal unxxqum : integer;
  signal geafvi : std_logic;
begin
  g : entity work.vbtj
    port map (jakkjcr => geafvi, msvqdb => unxxqum, atxi => xurnzsxkr, xiz => tqw);
  wkwyzgkr : entity work.q
    port map (gvbeepxo => k);
  uqslvw : entity work.q
    port map (gvbeepxo => er);
  rx : entity work.vbtj
    port map (jakkjcr => geafvi, msvqdb => xurnzsxkr, atxi => olcurz, xiz => tqw);
end wu;



-- Seed after: 14427419234275697506,11387579217500963635
