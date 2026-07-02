-- Seed: 17823303522762214521,13694093582652240945

library ieee;
use ieee.std_logic_1164.all;

entity vxavzut is
  port (cxklvpvq : linkage std_logic_vector(2 to 2); fgghh : buffer time_vector(4 to 3); bqblfr : in time_vector(3 downto 0));
end vxavzut;

architecture mrwgemdaba of vxavzut is
  
begin
  -- Single-driven assignments
  fgghh <= (others => 0 ns);
end mrwgemdaba;

library ieee;
use ieee.std_logic_1164.all;

entity oep is
  port (tcibkx : linkage std_logic);
end oep;

library ieee;
use ieee.std_logic_1164.all;

architecture md of oep is
  signal qji : time_vector(3 downto 0);
  signal mbdketqw : time_vector(4 to 3);
  signal mr : std_logic_vector(2 to 2);
  signal waskg : time_vector(4 to 3);
  signal skpvubcopc : time_vector(3 downto 0);
  signal acze : time_vector(4 to 3);
  signal dk : std_logic_vector(2 to 2);
  signal xyjcyau : time_vector(3 downto 0);
  signal vfnuu : time_vector(4 to 3);
  signal qrl : std_logic_vector(2 to 2);
begin
  zyrqzev : entity work.vxavzut
    port map (cxklvpvq => qrl, fgghh => vfnuu, bqblfr => xyjcyau);
  oxoss : entity work.vxavzut
    port map (cxklvpvq => dk, fgghh => acze, bqblfr => skpvubcopc);
  i : entity work.vxavzut
    port map (cxklvpvq => qrl, fgghh => waskg, bqblfr => skpvubcopc);
  ixyg : entity work.vxavzut
    port map (cxklvpvq => mr, fgghh => mbdketqw, bqblfr => qji);
  
  -- Multi-driven assignments
  mr <= (others => '0');
end md;

library ieee;
use ieee.std_logic_1164.all;

entity ry is
  port (vvfym : buffer std_logic);
end ry;

library ieee;
use ieee.std_logic_1164.all;

architecture fjlxgdbudi of ry is
  signal pd : time_vector(3 downto 0);
  signal tyotmyyqm : time_vector(4 to 3);
  signal spqvq : std_logic_vector(2 to 2);
begin
  qcx : entity work.vxavzut
    port map (cxklvpvq => spqvq, fgghh => tyotmyyqm, bqblfr => pd);
  
  -- Single-driven assignments
  pd <= (8#1_7_6_6_2.7# fs, 2#1.1_0# ps, 1 hr, 8#356# us);
  
  -- Multi-driven assignments
  spqvq <= (others => 'W');
  vvfym <= 'W';
  vvfym <= 'Z';
end fjlxgdbudi;



-- Seed after: 2392993187928037588,13694093582652240945
