-- Seed: 14160541196716943155,14629254427735353553

library ieee;
use ieee.std_logic_1164.all;

entity hihbw is
  port (xyzmfnmqe : linkage string(1 downto 2); gaio : buffer std_logic_vector(4 to 1); rsexrtm : in std_logic_vector(0 downto 0));
end hihbw;

architecture mwyemqj of hihbw is
  
begin
  -- Multi-driven assignments
  gaio <= (others => '0');
  gaio <= "";
end mwyemqj;

entity qim is
  port (uyzdnpnhe : inout boolean_vector(4 to 2); cmuevnrm : in boolean);
end qim;

library ieee;
use ieee.std_logic_1164.all;

architecture cdttg of qim is
  signal mwfzqdrhq : string(1 downto 2);
  signal u : std_logic_vector(0 downto 0);
  signal qmzdq : std_logic_vector(4 to 1);
  signal iqtzbugv : string(1 downto 2);
begin
  liftivxk : entity work.hihbw
    port map (xyzmfnmqe => iqtzbugv, gaio => qmzdq, rsexrtm => u);
  vciw : entity work.hihbw
    port map (xyzmfnmqe => mwfzqdrhq, gaio => qmzdq, rsexrtm => u);
  
  -- Single-driven assignments
  uyzdnpnhe <= (others => TRUE);
  
  -- Multi-driven assignments
  u <= "-";
  qmzdq <= (others => '0');
  u <= "U";
  qmzdq <= (others => '0');
end cdttg;



-- Seed after: 2241315551724222954,14629254427735353553
