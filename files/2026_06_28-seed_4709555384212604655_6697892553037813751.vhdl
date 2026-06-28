-- Seed: 4709555384212604655,6697892553037813751

entity hjcnlvsonb is
  port (hfy : inout real_vector(2 downto 1); umrylou : out boolean_vector(2 to 1); erdd : buffer real_vector(0 to 2));
end hjcnlvsonb;

architecture kwarz of hjcnlvsonb is
  
begin
  -- Single-driven assignments
  erdd <= (16#83.23A34#, 8#7_1_6.1_2_4#, 2_4_0_1_4.1);
  umrylou <= (others => TRUE);
  hfy <= (4_0.2210, 8#116.5054#);
end kwarz;

library ieee;
use ieee.std_logic_1164.all;

entity qbqlwf is
  port (kvd : inout std_logic; viijy : in std_logic);
end qbqlwf;

architecture owmdsw of qbqlwf is
  signal zyhzux : real_vector(0 to 2);
  signal rdkayl : boolean_vector(2 to 1);
  signal kpdwckqjbw : real_vector(2 downto 1);
  signal mdmfqjmary : real_vector(0 to 2);
  signal xlofiis : boolean_vector(2 to 1);
  signal squi : real_vector(2 downto 1);
begin
  at : entity work.hjcnlvsonb
    port map (hfy => squi, umrylou => xlofiis, erdd => mdmfqjmary);
  muboegl : entity work.hjcnlvsonb
    port map (hfy => kpdwckqjbw, umrylou => rdkayl, erdd => zyhzux);
  
  -- Multi-driven assignments
  kvd <= '1';
end owmdsw;

library ieee;
use ieee.std_logic_1164.all;

entity tbmp is
  port (ovdyimtm : out real; udbet : linkage time; thkuur : out std_logic_vector(0 to 2));
end tbmp;

architecture filgz of tbmp is
  
begin
  -- Multi-driven assignments
  thkuur <= ('L', '0', '-');
  thkuur <= ('H', 'Z', 'X');
  thkuur <= "Z0-";
end filgz;



-- Seed after: 13172712305653475740,6697892553037813751
