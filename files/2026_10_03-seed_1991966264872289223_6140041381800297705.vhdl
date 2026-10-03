-- Seed: 1991966264872289223,6140041381800297705

library ieee;
use ieee.std_logic_1164.all;

entity pneji is
  port (hecg : inout integer; zbqfjv : buffer std_logic_vector(0 to 4); rp : inout integer; ei : in integer);
end pneji;

architecture wjuhbcw of pneji is
  
begin
  -- Single-driven assignments
  hecg <= rp;
  rp <= ei;
  
  -- Multi-driven assignments
  zbqfjv <= zbqfjv;
  zbqfjv <= zbqfjv;
end wjuhbcw;

entity vcsq is
  port (pnwzz : linkage time_vector(1 downto 2); oc : buffer bit_vector(3 downto 4));
end vcsq;

library ieee;
use ieee.std_logic_1164.all;

architecture cctiht of vcsq is
  signal y : integer;
  signal pgyggo : std_logic_vector(0 to 4);
  signal abqjcoyd : integer;
  signal hwl : integer;
  signal omhb : std_logic_vector(0 to 4);
  signal sllckfp : integer;
  signal yjoxe : integer;
  signal t : std_logic_vector(0 to 4);
  signal foohrkgjk : integer;
  signal liiqs : integer;
  signal htlrzlj : integer;
  signal aasqfutjie : std_logic_vector(0 to 4);
  signal xbjbrki : integer;
begin
  ewocv : entity work.pneji
    port map (hecg => xbjbrki, zbqfjv => aasqfutjie, rp => htlrzlj, ei => liiqs);
  hba : entity work.pneji
    port map (hecg => foohrkgjk, zbqfjv => t, rp => yjoxe, ei => sllckfp);
  uvrmtoge : entity work.pneji
    port map (hecg => liiqs, zbqfjv => omhb, rp => sllckfp, ei => hwl);
  kfhudz : entity work.pneji
    port map (hecg => abqjcoyd, zbqfjv => pgyggo, rp => y, ei => htlrzlj);
  
  -- Single-driven assignments
  hwl <= y;
  oc <= (others => '0');
end cctiht;

library ieee;
use ieee.std_logic_1164.all;

entity isnnc is
  port (mu : in std_logic; otewljpgcc : inout time_vector(3 downto 3); j : inout time);
end isnnc;

library ieee;
use ieee.std_logic_1164.all;

architecture gx of isnnc is
  signal naamkuzc : integer;
  signal mbmhbnt : std_logic_vector(0 to 4);
  signal wcsaj : integer;
  signal zyccceg : bit_vector(3 downto 4);
  signal xdvrolyd : time_vector(1 downto 2);
  signal pdahtgcxp : bit_vector(3 downto 4);
  signal irkkukw : time_vector(1 downto 2);
begin
  ctwowwtqw : entity work.vcsq
    port map (pnwzz => irkkukw, oc => pdahtgcxp);
  pu : entity work.vcsq
    port map (pnwzz => xdvrolyd, oc => zyccceg);
  eqxsgakvf : entity work.pneji
    port map (hecg => wcsaj, zbqfjv => mbmhbnt, rp => naamkuzc, ei => wcsaj);
  
  -- Single-driven assignments
  otewljpgcc <= (others => 2#1_0# ps);
  j <= 1_3_3.1_3_4 fs;
  
  -- Multi-driven assignments
  mbmhbnt <= ('0', 'U', '1', 'L', 'H');
  mbmhbnt <= mbmhbnt;
end gx;

library ieee;
use ieee.std_logic_1164.all;

entity sfteqqh is
  port (djvq : inout real; zh : in bit; wciv : in std_logic_vector(3 downto 4); we : linkage integer);
end sfteqqh;

library ieee;
use ieee.std_logic_1164.all;

architecture a of sfteqqh is
  signal psbqqjg : time;
  signal qblh : time_vector(3 downto 3);
  signal kp : std_logic;
  signal ogjgvl : integer;
  signal yu : integer;
  signal sc : integer;
  signal pyksjccwdc : integer;
  signal syqem : std_logic_vector(0 to 4);
  signal wvgectmg : integer;
begin
  fj : entity work.pneji
    port map (hecg => wvgectmg, zbqfjv => syqem, rp => pyksjccwdc, ei => sc);
  ovclfrogm : entity work.pneji
    port map (hecg => yu, zbqfjv => syqem, rp => sc, ei => ogjgvl);
  ogep : entity work.isnnc
    port map (mu => kp, otewljpgcc => qblh, j => psbqqjg);
  
  -- Single-driven assignments
  djvq <= djvq;
  ogjgvl <= 30;
  
  -- Multi-driven assignments
  kp <= kp;
end a;



-- Seed after: 4505633687359503753,6140041381800297705
