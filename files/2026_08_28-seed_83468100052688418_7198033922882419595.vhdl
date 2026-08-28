-- Seed: 83468100052688418,7198033922882419595

entity gwwdiitxz is
  port (zncd : inout real_vector(1 downto 1); mrmcbdpyh : out severity_level; kaoxdiysuh : out time);
end gwwdiitxz;

architecture zhpwirniww of gwwdiitxz is
  
begin
  
end zhpwirniww;

library ieee;
use ieee.std_logic_1164.all;

entity dsplzzsl is
  port (i : buffer real; zlxxei : out time_vector(3 to 3); pbl : out time; ygi : buffer std_logic);
end dsplzzsl;

architecture fijeose of dsplzzsl is
  signal nduy : severity_level;
  signal f : real_vector(1 downto 1);
begin
  lzatckyven : entity work.gwwdiitxz
    port map (zncd => f, mrmcbdpyh => nduy, kaoxdiysuh => pbl);
  
  -- Single-driven assignments
  zlxxei <= (others => 8#262.7_7_4_2# us);
  i <= i;
end fijeose;

library ieee;
use ieee.std_logic_1164.all;

entity ockenqe is
  port (kzkwom : out std_logic_vector(4 to 1));
end ockenqe;

library ieee;
use ieee.std_logic_1164.all;

architecture uuhkyuqzw of ockenqe is
  signal eg : time;
  signal mxg : time_vector(3 to 3);
  signal meifmiuj : real;
  signal poipdswk : std_logic;
  signal bgcbiyory : time;
  signal sdfzb : time_vector(3 to 3);
  signal fqq : real;
  signal ftttf : time;
  signal vxegtaweld : severity_level;
  signal dqz : real_vector(1 downto 1);
  signal lwkb : std_logic;
  signal sp : time;
  signal nvrqqyoa : time_vector(3 to 3);
  signal dufiiul : real;
begin
  iyldmnax : entity work.dsplzzsl
    port map (i => dufiiul, zlxxei => nvrqqyoa, pbl => sp, ygi => lwkb);
  s : entity work.gwwdiitxz
    port map (zncd => dqz, mrmcbdpyh => vxegtaweld, kaoxdiysuh => ftttf);
  elrxs : entity work.dsplzzsl
    port map (i => fqq, zlxxei => sdfzb, pbl => bgcbiyory, ygi => poipdswk);
  z : entity work.dsplzzsl
    port map (i => meifmiuj, zlxxei => mxg, pbl => eg, ygi => poipdswk);
  
  -- Multi-driven assignments
  kzkwom <= kzkwom;
  poipdswk <= '0';
end uuhkyuqzw;

entity dxok is
  port (yioagb : buffer time; pg : out time; eauopnhy : out severity_level);
end dxok;

library ieee;
use ieee.std_logic_1164.all;

architecture z of dxok is
  signal n : real_vector(1 downto 1);
  signal t : std_logic_vector(4 to 1);
begin
  jb : entity work.ockenqe
    port map (kzkwom => t);
  sgones : entity work.gwwdiitxz
    port map (zncd => n, mrmcbdpyh => eauopnhy, kaoxdiysuh => pg);
  
  -- Multi-driven assignments
  t <= t;
  t <= t;
  t <= (others => '0');
end z;



-- Seed after: 18352172117225355725,7198033922882419595
