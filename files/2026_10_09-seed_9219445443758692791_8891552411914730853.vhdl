-- Seed: 9219445443758692791,8891552411914730853

library ieee;
use ieee.std_logic_1164.all;

entity cv is
  port (wyvsip : in real; o : buffer integer_vector(3 to 3); cg : out integer_vector(0 downto 2); l : buffer std_logic_vector(1 downto 1));
end cv;

architecture hg of cv is
  
begin
  -- Single-driven assignments
  cg <= (others => 0);
  o <= o;
end hg;

entity kkowrrcs is
  port (lowditzeg : out integer; tiotnbd : buffer real; vfhmarxvoz : out time);
end kkowrrcs;

library ieee;
use ieee.std_logic_1164.all;

architecture sbimbmgb of kkowrrcs is
  signal vkdehqulv : integer_vector(0 downto 2);
  signal kclyytp : integer_vector(3 to 3);
  signal azrs : integer_vector(0 downto 2);
  signal gtkwpjrp : integer_vector(3 to 3);
  signal muxuu : real;
  signal assvhdcuzf : std_logic_vector(1 downto 1);
  signal tlqnxnppp : integer_vector(0 downto 2);
  signal idacdbxv : integer_vector(3 to 3);
begin
  hscu : entity work.cv
    port map (wyvsip => tiotnbd, o => idacdbxv, cg => tlqnxnppp, l => assvhdcuzf);
  gwugexrs : entity work.cv
    port map (wyvsip => muxuu, o => gtkwpjrp, cg => azrs, l => assvhdcuzf);
  dxwf : entity work.cv
    port map (wyvsip => tiotnbd, o => kclyytp, cg => vkdehqulv, l => assvhdcuzf);
  
  -- Single-driven assignments
  lowditzeg <= lowditzeg;
  vfhmarxvoz <= 2#0_1.0_1_1# fs;
  
  -- Multi-driven assignments
  assvhdcuzf <= assvhdcuzf;
  assvhdcuzf <= "L";
  assvhdcuzf <= (others => 'L');
  assvhdcuzf <= (others => '1');
end sbimbmgb;



-- Seed after: 14181035636133506532,8891552411914730853
