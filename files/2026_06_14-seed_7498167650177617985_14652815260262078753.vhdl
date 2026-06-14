-- Seed: 7498167650177617985,14652815260262078753

library ieee;
use ieee.std_logic_1164.all;

entity kcya is
  port (owmalpm : buffer std_logic_vector(4 downto 3); wakd : buffer integer; j : inout boolean_vector(4 downto 3));
end kcya;

architecture avco of kcya is
  
begin
  -- Multi-driven assignments
  owmalpm <= "0X";
  owmalpm <= ('-', '-');
  owmalpm <= ('H', 'U');
end avco;

library ieee;
use ieee.std_logic_1164.all;

entity nbrkxx is
  port (dcgi : buffer real_vector(3 downto 3); svuarju : inout std_logic);
end nbrkxx;

library ieee;
use ieee.std_logic_1164.all;

architecture trxropb of nbrkxx is
  signal su : boolean_vector(4 downto 3);
  signal iteq : integer;
  signal lrltbwfmdj : std_logic_vector(4 downto 3);
  signal dyx : boolean_vector(4 downto 3);
  signal mwhaxmziq : integer;
  signal fxes : std_logic_vector(4 downto 3);
begin
  wzwfckzfu : entity work.kcya
    port map (owmalpm => fxes, wakd => mwhaxmziq, j => dyx);
  rxqnki : entity work.kcya
    port map (owmalpm => lrltbwfmdj, wakd => iteq, j => su);
  
  -- Single-driven assignments
  dcgi <= (others => 1002.241);
end trxropb;

library ieee;
use ieee.std_logic_1164.all;

entity gp is
  port (evblb : linkage time; mdakkpqwvm : in std_logic);
end gp;

library ieee;
use ieee.std_logic_1164.all;

architecture wtetv of gp is
  signal vxhqdhoy : real_vector(3 downto 3);
  signal v : std_logic;
  signal tywynw : real_vector(3 downto 3);
begin
  ffhux : entity work.nbrkxx
    port map (dcgi => tywynw, svuarju => v);
  hlzmwr : entity work.nbrkxx
    port map (dcgi => vxhqdhoy, svuarju => v);
  
  -- Multi-driven assignments
  v <= '-';
end wtetv;



-- Seed after: 8549621512891202808,14652815260262078753
