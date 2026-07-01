-- Seed: 10978770180209638842,6882842853887419669

entity c is
  port (vxuxbv : out time);
end c;

architecture qudyw of c is
  
begin
  
end qudyw;

library ieee;
use ieee.std_logic_1164.all;

entity cmj is
  port (wp : out time; lzdrqyin : buffer time; teqkuov : in boolean_vector(0 downto 2); lkb : inout std_logic);
end cmj;

architecture pviyaremq of cmj is
  
begin
  qgewfinp : entity work.c
    port map (vxuxbv => lzdrqyin);
  o : entity work.c
    port map (vxuxbv => wp);
  
  -- Multi-driven assignments
  lkb <= 'X';
  lkb <= 'L';
  lkb <= '-';
end pviyaremq;

entity egdnlqucrm is
  port (dsgqmojsss : buffer integer; bbmllcdma : buffer time);
end egdnlqucrm;

architecture xdee of egdnlqucrm is
  signal mtduptjqwb : time;
  signal bbzcz : time;
begin
  zw : entity work.c
    port map (vxuxbv => bbzcz);
  irubunzsh : entity work.c
    port map (vxuxbv => bbmllcdma);
  hwupjokgg : entity work.c
    port map (vxuxbv => mtduptjqwb);
  
  -- Single-driven assignments
  dsgqmojsss <= 8#5#;
end xdee;



-- Seed after: 12370206121291289109,6882842853887419669
