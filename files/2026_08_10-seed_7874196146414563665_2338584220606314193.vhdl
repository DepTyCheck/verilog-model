-- Seed: 7874196146414563665,2338584220606314193

entity mhc is
  port (tdmdkz : out real_vector(4 to 0); svgvysssxj : in bit; sdj : linkage severity_level; kbwrp : in severity_level);
end mhc;

architecture iervra of mhc is
  
begin
  -- Single-driven assignments
  tdmdkz <= (others => 0.0);
end iervra;

entity ythbfap is
  port (nxzk : inout integer);
end ythbfap;

architecture gd of ythbfap is
  signal ffu : severity_level;
  signal jxhdac : bit;
  signal prqayomcbo : real_vector(4 to 0);
  signal rhwdhci : severity_level;
  signal dltrpz : bit;
  signal tntwcmid : real_vector(4 to 0);
  signal hwycl : severity_level;
  signal inolwm : bit;
  signal jnszdmj : real_vector(4 to 0);
  signal ezvnbq : severity_level;
  signal mqfg : severity_level;
  signal avm : bit;
  signal sbwkz : real_vector(4 to 0);
begin
  ou : entity work.mhc
    port map (tdmdkz => sbwkz, svgvysssxj => avm, sdj => mqfg, kbwrp => ezvnbq);
  tnvvqlkcy : entity work.mhc
    port map (tdmdkz => jnszdmj, svgvysssxj => inolwm, sdj => hwycl, kbwrp => ezvnbq);
  pngz : entity work.mhc
    port map (tdmdkz => tntwcmid, svgvysssxj => dltrpz, sdj => rhwdhci, kbwrp => rhwdhci);
  b : entity work.mhc
    port map (tdmdkz => prqayomcbo, svgvysssxj => jxhdac, sdj => ffu, kbwrp => mqfg);
  
  -- Single-driven assignments
  nxzk <= nxzk;
  inolwm <= avm;
end gd;



-- Seed after: 12931153797401424423,2338584220606314193
