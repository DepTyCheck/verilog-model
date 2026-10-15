-- Seed: 17879257593996231113,6377581586043967497

library ieee;
use ieee.std_logic_1164.all;

entity sb is
  port (v : buffer time; o : buffer std_logic);
end sb;

architecture uw of sb is
  
begin
  -- Single-driven assignments
  v <= 12.1_1_2 us;
  
  -- Multi-driven assignments
  o <= o;
  o <= '-';
  o <= o;
  o <= 'U';
end uw;

library ieee;
use ieee.std_logic_1164.all;

entity qbxrg is
  port (hvrhkxg : in integer; ign : out std_logic; udduuibp : in real);
end qbxrg;

library ieee;
use ieee.std_logic_1164.all;

architecture grmk of qbxrg is
  signal l : time;
  signal fxp : time;
  signal ql : std_logic;
  signal jqrmflcq : time;
  signal bitcneugb : std_logic;
  signal wlbckdnua : time;
begin
  fpuyagqkh : entity work.sb
    port map (v => wlbckdnua, o => bitcneugb);
  xfydvufjp : entity work.sb
    port map (v => jqrmflcq, o => ql);
  z : entity work.sb
    port map (v => fxp, o => ign);
  fepeukkes : entity work.sb
    port map (v => l, o => ign);
  
  -- Multi-driven assignments
  ign <= 'H';
  ign <= '0';
end grmk;

library ieee;
use ieee.std_logic_1164.all;

entity xq is
  port (zzks : buffer severity_level; nt : inout real; bhbcsmvpj : linkage std_logic_vector(4 downto 2));
end xq;

library ieee;
use ieee.std_logic_1164.all;

architecture fey of xq is
  signal varllbcfp : std_logic;
  signal yoosbzmsdo : time;
  signal uvgechtl : std_logic;
  signal wxbiehbzj : time;
begin
  okbjliwswo : entity work.sb
    port map (v => wxbiehbzj, o => uvgechtl);
  cw : entity work.sb
    port map (v => yoosbzmsdo, o => varllbcfp);
  
  -- Single-driven assignments
  nt <= nt;
  zzks <= WARNING;
  
  -- Multi-driven assignments
  varllbcfp <= 'H';
  uvgechtl <= '0';
  uvgechtl <= varllbcfp;
end fey;

entity ucfjdp is
  port (qjunhkpga : inout real);
end ucfjdp;

library ieee;
use ieee.std_logic_1164.all;

architecture gthpig of ucfjdp is
  signal zsj : std_logic;
  signal ex : time;
  signal wtocdzlpcl : std_logic;
  signal jvmovhz : real;
  signal ahkcpvi : std_logic;
  signal nxqynuj : integer;
  signal ujygddd : std_logic;
  signal sijpaoghw : integer;
begin
  k : entity work.qbxrg
    port map (hvrhkxg => sijpaoghw, ign => ujygddd, udduuibp => qjunhkpga);
  zw : entity work.qbxrg
    port map (hvrhkxg => nxqynuj, ign => ahkcpvi, udduuibp => jvmovhz);
  geq : entity work.qbxrg
    port map (hvrhkxg => sijpaoghw, ign => wtocdzlpcl, udduuibp => qjunhkpga);
  cqou : entity work.sb
    port map (v => ex, o => zsj);
  
  -- Single-driven assignments
  qjunhkpga <= qjunhkpga;
end gthpig;



-- Seed after: 4960164375751644193,6377581586043967497
