-- Seed: 368346266512361220,6377581586043967497

library ieee;
use ieee.std_logic_1164.all;

entity jwqcju is
  port (idmcpxqe : out std_logic_vector(0 downto 1); glscudb : inout std_logic_vector(4 to 0); yzqwkluw : inout severity_level; pzs : inout time);
end jwqcju;

architecture lyfuv of jwqcju is
  
begin
  -- Single-driven assignments
  yzqwkluw <= ERROR;
  pzs <= 2#101.10101# fs;
  
  -- Multi-driven assignments
  glscudb <= "";
  idmcpxqe <= (others => '0');
end lyfuv;

entity naawm is
  port (gjg : out character; boqpuevo : inout time; w : in real; sj : in real);
end naawm;

library ieee;
use ieee.std_logic_1164.all;

architecture scjs of naawm is
  signal jnh : severity_level;
  signal robcd : std_logic_vector(4 to 0);
  signal tetzj : time;
  signal yvtyplqnwr : severity_level;
  signal zoxqlu : std_logic_vector(0 downto 1);
  signal lcvloxqon : time;
  signal k : severity_level;
  signal tjpnobm : std_logic_vector(4 to 0);
  signal mfnvkakzqj : std_logic_vector(4 to 0);
begin
  rdcuy : entity work.jwqcju
    port map (idmcpxqe => mfnvkakzqj, glscudb => tjpnobm, yzqwkluw => k, pzs => lcvloxqon);
  mi : entity work.jwqcju
    port map (idmcpxqe => zoxqlu, glscudb => mfnvkakzqj, yzqwkluw => yvtyplqnwr, pzs => tetzj);
  g : entity work.jwqcju
    port map (idmcpxqe => zoxqlu, glscudb => robcd, yzqwkluw => jnh, pzs => boqpuevo);
  
  -- Single-driven assignments
  gjg <= 'j';
  
  -- Multi-driven assignments
  zoxqlu <= (others => '0');
end scjs;

library ieee;
use ieee.std_logic_1164.all;

entity a is
  port (tjtfdso : linkage std_logic; haryezc : buffer time_vector(2 downto 1));
end a;

library ieee;
use ieee.std_logic_1164.all;

architecture tcwfohznit of a is
  signal qwxd : time;
  signal vyp : severity_level;
  signal dyflcpi : time;
  signal hxflbmp : character;
  signal oegeegbfc : time;
  signal newwbjxua : severity_level;
  signal j : std_logic_vector(4 to 0);
  signal ayycgi : real;
  signal nagfqrtww : real;
  signal pwwax : time;
  signal mawbq : character;
begin
  vxciv : entity work.naawm
    port map (gjg => mawbq, boqpuevo => pwwax, w => nagfqrtww, sj => ayycgi);
  kpvxyvxnth : entity work.jwqcju
    port map (idmcpxqe => j, glscudb => j, yzqwkluw => newwbjxua, pzs => oegeegbfc);
  przvdo : entity work.naawm
    port map (gjg => hxflbmp, boqpuevo => dyflcpi, w => nagfqrtww, sj => nagfqrtww);
  waqgw : entity work.jwqcju
    port map (idmcpxqe => j, glscudb => j, yzqwkluw => vyp, pzs => qwxd);
  
  -- Single-driven assignments
  ayycgi <= nagfqrtww;
  nagfqrtww <= 1_3.40;
  haryezc <= (2 min, 4 sec);
  
  -- Multi-driven assignments
  j <= j;
  j <= "";
  j <= j;
end tcwfohznit;



-- Seed after: 3687446257322429273,6377581586043967497
