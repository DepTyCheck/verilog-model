-- Seed: 4050230916037637435,6697892553037813751

library ieee;
use ieee.std_logic_1164.all;

entity qsrr is
  port (rwxo : in time; xsyar : out std_logic_vector(3 downto 2); fakciv : buffer bit);
end qsrr;

architecture nltarnvqr of qsrr is
  
begin
  -- Single-driven assignments
  fakciv <= '0';
  
  -- Multi-driven assignments
  xsyar <= ('U', 'X');
  xsyar <= ('U', '-');
end nltarnvqr;

library ieee;
use ieee.std_logic_1164.all;

entity htiwiu is
  port (evyvsutwkj : out std_logic_vector(1 downto 1); vmod : linkage bit_vector(4 to 4));
end htiwiu;

library ieee;
use ieee.std_logic_1164.all;

architecture jxpkjaevj of htiwiu is
  signal boriat : bit;
  signal za : time;
  signal fiw : bit;
  signal uadmudd : time;
  signal rlekww : bit;
  signal ubxqyghegx : std_logic_vector(3 downto 2);
  signal bb : time;
begin
  kg : entity work.qsrr
    port map (rwxo => bb, xsyar => ubxqyghegx, fakciv => rlekww);
  o : entity work.qsrr
    port map (rwxo => uadmudd, xsyar => ubxqyghegx, fakciv => fiw);
  odsrtmpkh : entity work.qsrr
    port map (rwxo => za, xsyar => ubxqyghegx, fakciv => boriat);
  
  -- Single-driven assignments
  uadmudd <= 2#10010# us;
  bb <= 2#0.1_1_0# us;
  
  -- Multi-driven assignments
  evyvsutwkj <= (others => '1');
  evyvsutwkj <= (others => 'W');
  evyvsutwkj <= (others => 'Z');
  evyvsutwkj <= "L";
end jxpkjaevj;

entity gbrp is
  port (cbud : inout time_vector(1 downto 1));
end gbrp;

library ieee;
use ieee.std_logic_1164.all;

architecture g of gbrp is
  signal yapmljlsrb : bit_vector(4 to 4);
  signal eeup : std_logic_vector(1 downto 1);
  signal qalrppv : bit;
  signal vmz : std_logic_vector(3 downto 2);
  signal adgfpfje : bit_vector(4 to 4);
  signal o : std_logic_vector(1 downto 1);
  signal uvd : bit;
  signal e : std_logic_vector(3 downto 2);
  signal tvcngg : time;
begin
  bvdqisyw : entity work.qsrr
    port map (rwxo => tvcngg, xsyar => e, fakciv => uvd);
  sa : entity work.htiwiu
    port map (evyvsutwkj => o, vmod => adgfpfje);
  zjlntwr : entity work.qsrr
    port map (rwxo => tvcngg, xsyar => vmz, fakciv => qalrppv);
  igci : entity work.htiwiu
    port map (evyvsutwkj => eeup, vmod => yapmljlsrb);
  
  -- Single-driven assignments
  cbud <= (others => 1 hr);
  tvcngg <= 16#D4A0B.5_3_1# ns;
  
  -- Multi-driven assignments
  e <= ('L', 'U');
  e <= "0W";
  eeup <= (others => 'Z');
end g;



-- Seed after: 5841353106630909530,6697892553037813751
