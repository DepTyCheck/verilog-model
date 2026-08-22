-- Seed: 12247934226452760115,5805648483995786113

entity mfitt is
  port (oig : out bit_vector(1 to 2));
end mfitt;

architecture wltonpopri of mfitt is
  
begin
  -- Single-driven assignments
  oig <= ('0', '1');
end wltonpopri;

library ieee;
use ieee.std_logic_1164.all;

entity ushujfusf is
  port (sewouio : out integer; i : in boolean; xybtjnvno : buffer std_logic);
end ushujfusf;

architecture qsxtbcfe of ushujfusf is
  signal krwuhnt : bit_vector(1 to 2);
  signal bzabkx : bit_vector(1 to 2);
  signal qv : bit_vector(1 to 2);
begin
  ltjsfmbru : entity work.mfitt
    port map (oig => qv);
  iytmmkkf : entity work.mfitt
    port map (oig => bzabkx);
  wygnflyu : entity work.mfitt
    port map (oig => krwuhnt);
  
  -- Multi-driven assignments
  xybtjnvno <= 'L';
end qsxtbcfe;

library ieee;
use ieee.std_logic_1164.all;

entity uji is
  port (c : inout std_logic_vector(3 downto 4));
end uji;

architecture bbjq of uji is
  signal crrsqe : bit_vector(1 to 2);
  signal voamzcbcq : bit_vector(1 to 2);
begin
  hfhlgccv : entity work.mfitt
    port map (oig => voamzcbcq);
  hidzfyefzj : entity work.mfitt
    port map (oig => crrsqe);
  
  -- Multi-driven assignments
  c <= c;
  c <= "";
end bbjq;



-- Seed after: 13032321687988670552,5805648483995786113
