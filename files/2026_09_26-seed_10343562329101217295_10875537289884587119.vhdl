-- Seed: 10343562329101217295,10875537289884587119

library ieee;
use ieee.std_logic_1164.all;

entity goc is
  port (ldirw : out boolean_vector(2 to 1); plvfofrypg : out real_vector(2 downto 3); t : in std_logic_vector(3 to 0); ysiigluzxm : inout std_logic);
end goc;

architecture ar of goc is
  
begin
  -- Single-driven assignments
  plvfofrypg <= plvfofrypg;
  ldirw <= (others => TRUE);
  
  -- Multi-driven assignments
  ysiigluzxm <= '1';
  ysiigluzxm <= ysiigluzxm;
  ysiigluzxm <= ysiigluzxm;
end ar;

library ieee;
use ieee.std_logic_1164.all;

entity wrmoplb is
  port (vhfcsf : linkage boolean_vector(1 downto 2); gzz : in std_logic_vector(2 to 1); zz : inout time_vector(0 downto 1));
end wrmoplb;

architecture ecz of wrmoplb is
  
begin
  -- Single-driven assignments
  zz <= (others => 0 ns);
end ecz;

library ieee;
use ieee.std_logic_1164.all;

entity atg is
  port (jkueic : inout integer; eppzra : in std_logic);
end atg;

library ieee;
use ieee.std_logic_1164.all;

architecture p of atg is
  signal ubog : real_vector(2 downto 3);
  signal cmxsrvosx : boolean_vector(2 to 1);
  signal ev : std_logic;
  signal xkcom : std_logic_vector(3 to 0);
  signal hrlcs : real_vector(2 downto 3);
  signal dgqbusf : boolean_vector(2 to 1);
begin
  jnbkgpn : entity work.goc
    port map (ldirw => dgqbusf, plvfofrypg => hrlcs, t => xkcom, ysiigluzxm => ev);
  ic : entity work.goc
    port map (ldirw => cmxsrvosx, plvfofrypg => ubog, t => xkcom, ysiigluzxm => ev);
  
  -- Single-driven assignments
  jkueic <= jkueic;
  
  -- Multi-driven assignments
  xkcom <= (others => '0');
end p;



-- Seed after: 1025654005020308090,10875537289884587119
