-- Seed: 3314232387091911358,15300320181035395489

entity y is
  port (pow : linkage real; dx : inout integer_vector(4 downto 0));
end y;

architecture c of y is
  
begin
  -- Single-driven assignments
  dx <= (16#5_3_7_F_3#, 16#B_B#, 16#E_2_7_1#, 16#4B3FA#, 2#1_0_0_0_1#);
end c;

library ieee;
use ieee.std_logic_1164.all;

entity qdliq is
  port (cxpkxem : out std_logic_vector(1 to 4); xtsa : out real; ytqrbh : in std_logic_vector(1 to 3); edob : linkage std_logic_vector(0 downto 0));
end qdliq;

architecture bfd of qdliq is
  signal jrijjqqo : integer_vector(4 downto 0);
begin
  euqvilr : entity work.y
    port map (pow => xtsa, dx => jrijjqqo);
  
  -- Multi-driven assignments
  cxpkxem <= "ZXXZ";
  cxpkxem <= "1HUU";
  cxpkxem <= ('Z', 'L', 'W', 'H');
end bfd;

entity bu is
  port (zcqo : in time);
end bu;

architecture yjcamxht of bu is
  signal zrvgdlmk : integer_vector(4 downto 0);
  signal gtokbcew : real;
  signal a : integer_vector(4 downto 0);
  signal ettchyl : real;
  signal vfwsffj : integer_vector(4 downto 0);
  signal gxs : real;
  signal elp : integer_vector(4 downto 0);
  signal fldty : real;
begin
  lgtobucq : entity work.y
    port map (pow => fldty, dx => elp);
  bsmngte : entity work.y
    port map (pow => gxs, dx => vfwsffj);
  tfsjui : entity work.y
    port map (pow => ettchyl, dx => a);
  igkg : entity work.y
    port map (pow => gtokbcew, dx => zrvgdlmk);
end yjcamxht;



-- Seed after: 18155039857898974886,15300320181035395489
