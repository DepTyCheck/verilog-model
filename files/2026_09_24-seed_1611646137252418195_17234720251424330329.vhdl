-- Seed: 1611646137252418195,17234720251424330329

library ieee;
use ieee.std_logic_1164.all;

entity xg is
  port (ns : linkage real; hqyb : buffer std_logic_vector(3 to 2));
end xg;

architecture xui of xg is
  
begin
  -- Multi-driven assignments
  hqyb <= "";
  hqyb <= (others => '0');
end xui;

entity rrvpsjqbck is
  port (ttoz : out real; vogzot : linkage real; zl : buffer integer; iuwamax : buffer boolean_vector(1 to 2));
end rrvpsjqbck;

library ieee;
use ieee.std_logic_1164.all;

architecture s of rrvpsjqbck is
  signal nwfn : real;
  signal nbfmtdv : std_logic_vector(3 to 2);
  signal eviwgovqs : real;
begin
  wcfc : entity work.xg
    port map (ns => eviwgovqs, hqyb => nbfmtdv);
  sbwlygfey : entity work.xg
    port map (ns => vogzot, hqyb => nbfmtdv);
  nwspotm : entity work.xg
    port map (ns => nwfn, hqyb => nbfmtdv);
  
  -- Single-driven assignments
  iuwamax <= (TRUE, FALSE);
  ttoz <= nwfn;
  zl <= 16#E#;
  
  -- Multi-driven assignments
  nbfmtdv <= "";
end s;



-- Seed after: 6930040064989818613,17234720251424330329
