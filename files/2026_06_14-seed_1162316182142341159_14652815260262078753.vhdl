-- Seed: 1162316182142341159,14652815260262078753

library ieee;
use ieee.std_logic_1164.all;

entity vgplaijo is
  port (g : out integer; iodfpsinb : out std_logic);
end vgplaijo;

architecture johp of vgplaijo is
  
begin
  -- Single-driven assignments
  g <= 40;
  
  -- Multi-driven assignments
  iodfpsinb <= '-';
  iodfpsinb <= 'U';
end johp;

library ieee;
use ieee.std_logic_1164.all;

entity yjdumv is
  port (qzpcyjfp : inout std_logic_vector(3 downto 2); rzsiyau : in bit; vu : buffer std_logic_vector(2 to 0); syqjzge : out character);
end yjdumv;

architecture zcgsto of yjdumv is
  
begin
  -- Single-driven assignments
  syqjzge <= 'e';
  
  -- Multi-driven assignments
  vu <= "";
  qzpcyjfp <= ('U', 'X');
  qzpcyjfp <= ('Z', 'W');
  qzpcyjfp <= ('L', 'L');
end zcgsto;

entity r is
  port (ahiosdkg : inout real; xguapj : buffer real; adeuid : out bit_vector(2 to 3); rgozppe : inout boolean_vector(2 downto 3));
end r;

library ieee;
use ieee.std_logic_1164.all;

architecture ofuplyo of r is
  signal mxm : integer;
  signal ikaiv : std_logic;
  signal garefxst : integer;
begin
  g : entity work.vgplaijo
    port map (g => garefxst, iodfpsinb => ikaiv);
  xqqb : entity work.vgplaijo
    port map (g => mxm, iodfpsinb => ikaiv);
  
  -- Single-driven assignments
  rgozppe <= (others => TRUE);
  ahiosdkg <= 16#7.924#;
  adeuid <= ('1', '1');
  xguapj <= 8#0_0_4_7_1.4#;
  
  -- Multi-driven assignments
  ikaiv <= 'Z';
  ikaiv <= 'L';
  ikaiv <= '1';
end ofuplyo;



-- Seed after: 12625205586089752461,14652815260262078753
