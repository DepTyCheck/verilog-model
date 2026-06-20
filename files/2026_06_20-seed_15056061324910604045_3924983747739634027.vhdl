-- Seed: 15056061324910604045,3924983747739634027

entity bqdhmtua is
  port (pjgxqsoai : out real_vector(3 downto 0); oqyyirnsa : in time);
end bqdhmtua;

architecture vnupf of bqdhmtua is
  
begin
  -- Single-driven assignments
  pjgxqsoai <= (2_3.00422, 3_1.03304, 0_2_4_1.11, 8#1526.5_5_1_3_4#);
end vnupf;

entity iykqfbaoc is
  port (iqbl : in real);
end iykqfbaoc;

architecture vcfley of iykqfbaoc is
  signal ossoyq : time;
  signal dxytm : real_vector(3 downto 0);
begin
  rnfgsove : entity work.bqdhmtua
    port map (pjgxqsoai => dxytm, oqyyirnsa => ossoyq);
  
  -- Single-driven assignments
  ossoyq <= 8#6_6_5.1223# fs;
end vcfley;

library ieee;
use ieee.std_logic_1164.all;

entity bpm is
  port (fnyc : inout severity_level; p : buffer integer; hslp : linkage std_logic; nzjcbhor : out real_vector(4 to 3));
end bpm;

architecture pww of bpm is
  signal jfbpxadp : time;
  signal wvls : real_vector(3 downto 0);
begin
  f : entity work.bqdhmtua
    port map (pjgxqsoai => wvls, oqyyirnsa => jfbpxadp);
  
  -- Single-driven assignments
  jfbpxadp <= 12230.1_4_0_1 fs;
  nzjcbhor <= (others => 0.0);
  p <= 2#1_0_0_1_1#;
end pww;



-- Seed after: 3125367989534191052,3924983747739634027
