-- Seed: 2968398076303684143,8437298063418820479

library ieee;
use ieee.std_logic_1164.all;

entity f is
  port (smefssqbpx : out real_vector(3 to 4); xwriiqynsr : inout time; nnmjdw : out severity_level; dkfvk : out std_logic_vector(0 to 2));
end f;

architecture dxezlotea of f is
  
begin
  -- Single-driven assignments
  nnmjdw <= WARNING;
end dxezlotea;

entity bstv is
  port (vpwnusexec : linkage boolean_vector(1 downto 0); ic : buffer real);
end bstv;

library ieee;
use ieee.std_logic_1164.all;

architecture nwndwb of bstv is
  signal ogclpiprb : severity_level;
  signal rtkweonbul : time;
  signal aruagn : real_vector(3 to 4);
  signal lpt : std_logic_vector(0 to 2);
  signal ycqdlkjhra : severity_level;
  signal o : time;
  signal sjjupnim : real_vector(3 to 4);
  signal zts : std_logic_vector(0 to 2);
  signal nalxcaqu : severity_level;
  signal apaaykpsbf : time;
  signal ovclnh : real_vector(3 to 4);
begin
  hnbsjhjnv : entity work.f
    port map (smefssqbpx => ovclnh, xwriiqynsr => apaaykpsbf, nnmjdw => nalxcaqu, dkfvk => zts);
  qidkzbe : entity work.f
    port map (smefssqbpx => sjjupnim, xwriiqynsr => o, nnmjdw => ycqdlkjhra, dkfvk => lpt);
  unuk : entity work.f
    port map (smefssqbpx => aruagn, xwriiqynsr => rtkweonbul, nnmjdw => ogclpiprb, dkfvk => zts);
  
  -- Single-driven assignments
  ic <= ic;
end nwndwb;



-- Seed after: 7214447098218095616,8437298063418820479
