-- Seed: 1955485687089200702,15300320181035395489

entity ofw is
  port (euogxp : buffer boolean);
end ofw;

architecture va of ofw is
  
begin
  -- Single-driven assignments
  euogxp <= TRUE;
end va;

library ieee;
use ieee.std_logic_1164.all;

entity vlkhjzmp is
  port (vl : buffer std_logic; baiknne : buffer integer_vector(2 downto 3); ilsx : linkage integer_vector(3 downto 2));
end vlkhjzmp;

architecture ulwcellok of vlkhjzmp is
  signal camsuknt : boolean;
  signal iwvcsq : boolean;
  signal fsffl : boolean;
  signal jdhcnq : boolean;
begin
  gti : entity work.ofw
    port map (euogxp => jdhcnq);
  xbvqhige : entity work.ofw
    port map (euogxp => fsffl);
  yccnx : entity work.ofw
    port map (euogxp => iwvcsq);
  wuokr : entity work.ofw
    port map (euogxp => camsuknt);
  
  -- Single-driven assignments
  baiknne <= (others => 0);
  
  -- Multi-driven assignments
  vl <= '0';
  vl <= 'W';
end ulwcellok;

library ieee;
use ieee.std_logic_1164.all;

entity xwit is
  port (tz : in std_logic_vector(4 downto 2); ncxra : linkage real; vjx : buffer integer; axmnclc : linkage std_logic_vector(0 to 0));
end xwit;

library ieee;
use ieee.std_logic_1164.all;

architecture jjmo of xwit is
  signal cmfps : boolean;
  signal cvdnnmyny : boolean;
  signal rwpi : integer_vector(3 downto 2);
  signal yhut : integer_vector(2 downto 3);
  signal wiyb : std_logic;
begin
  ijunkv : entity work.vlkhjzmp
    port map (vl => wiyb, baiknne => yhut, ilsx => rwpi);
  egttwgitws : entity work.ofw
    port map (euogxp => cvdnnmyny);
  lxuizgyhu : entity work.ofw
    port map (euogxp => cmfps);
  
  -- Single-driven assignments
  vjx <= 0;
  
  -- Multi-driven assignments
  wiyb <= 'L';
  wiyb <= '1';
end jjmo;

library ieee;
use ieee.std_logic_1164.all;

entity g is
  port (dmwi : in std_logic_vector(0 to 1));
end g;

library ieee;
use ieee.std_logic_1164.all;

architecture agu of g is
  signal mfsrcquf : boolean;
  signal mdqitzf : boolean;
  signal jcncs : integer_vector(3 downto 2);
  signal smmzahdge : integer_vector(2 downto 3);
  signal jm : integer_vector(3 downto 2);
  signal ugixuzrz : integer_vector(2 downto 3);
  signal sc : std_logic;
begin
  azcaxibu : entity work.vlkhjzmp
    port map (vl => sc, baiknne => ugixuzrz, ilsx => jm);
  qytglfz : entity work.vlkhjzmp
    port map (vl => sc, baiknne => smmzahdge, ilsx => jcncs);
  si : entity work.ofw
    port map (euogxp => mdqitzf);
  zoyhljpz : entity work.ofw
    port map (euogxp => mfsrcquf);
  
  -- Multi-driven assignments
  sc <= 'X';
end agu;



-- Seed after: 16675668091184254568,15300320181035395489
