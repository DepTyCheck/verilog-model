-- Seed: 13746515751065464261,6140041381800297705

entity xbjroa is
  port (doetf : linkage boolean_vector(2 to 4));
end xbjroa;

architecture gabt of xbjroa is
  
begin
  
end gabt;

entity ncklfngtfe is
  port (vailiy : buffer bit; rbxtszm : buffer real);
end ncklfngtfe;

architecture ys of ncklfngtfe is
  signal cjzrtwy : boolean_vector(2 to 4);
  signal mzvbckopzc : boolean_vector(2 to 4);
  signal nefehob : boolean_vector(2 to 4);
begin
  o : entity work.xbjroa
    port map (doetf => nefehob);
  qkltx : entity work.xbjroa
    port map (doetf => mzvbckopzc);
  imvhhlncax : entity work.xbjroa
    port map (doetf => cjzrtwy);
end ys;

library ieee;
use ieee.std_logic_1164.all;

entity fqmphhi is
  port (ukirls : inout std_logic_vector(1 downto 0); muedr : buffer real);
end fqmphhi;

architecture chqwxxeo of fqmphhi is
  signal ijze : bit;
  signal xkoslthdlh : boolean_vector(2 to 4);
  signal w : boolean_vector(2 to 4);
begin
  refpipzdye : entity work.xbjroa
    port map (doetf => w);
  tiqg : entity work.xbjroa
    port map (doetf => xkoslthdlh);
  oyvz : entity work.ncklfngtfe
    port map (vailiy => ijze, rbxtszm => muedr);
  
  -- Multi-driven assignments
  ukirls <= ukirls;
  ukirls <= ukirls;
end chqwxxeo;



-- Seed after: 8820096525582062422,6140041381800297705
