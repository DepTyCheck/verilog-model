-- Seed: 8974746294990403045,13843488114570579517

entity vcxieq is
  port (pc : out real; gvwe : buffer severity_level);
end vcxieq;

architecture frhjqdbv of vcxieq is
  
begin
  -- Single-driven assignments
  gvwe <= FAILURE;
  pc <= pc;
end frhjqdbv;

library ieee;
use ieee.std_logic_1164.all;

entity o is
  port (esmhgzf : linkage time; xfxosmw : buffer std_logic; kdjfwoz : buffer std_logic; fd : out std_logic_vector(2 to 0));
end o;

architecture ydk of o is
  signal sofppvk : severity_level;
  signal wbmq : real;
  signal tojze : severity_level;
  signal smsgaiuhsw : real;
  signal sxdrj : severity_level;
  signal merbhxujex : real;
begin
  svvyqa : entity work.vcxieq
    port map (pc => merbhxujex, gvwe => sxdrj);
  tehpfgxf : entity work.vcxieq
    port map (pc => smsgaiuhsw, gvwe => tojze);
  rqo : entity work.vcxieq
    port map (pc => wbmq, gvwe => sofppvk);
  
  -- Multi-driven assignments
  fd <= fd;
  xfxosmw <= '-';
  fd <= fd;
end ydk;

library ieee;
use ieee.std_logic_1164.all;

entity cunckgeas is
  port (sruup : out std_logic; zku : out boolean; hrng : out bit);
end cunckgeas;

library ieee;
use ieee.std_logic_1164.all;

architecture lvkc of cunckgeas is
  signal lf : std_logic;
  signal dfeslbzmfg : std_logic;
  signal qgbbzmwy : time;
  signal qyu : std_logic_vector(2 to 0);
  signal jmcc : std_logic;
  signal zlya : time;
  signal fwc : severity_level;
  signal qmvagjnfi : real;
  signal pkjduhlyne : std_logic_vector(2 to 0);
  signal ygogbkc : time;
begin
  gqivosogys : entity work.o
    port map (esmhgzf => ygogbkc, xfxosmw => sruup, kdjfwoz => sruup, fd => pkjduhlyne);
  jztfabf : entity work.vcxieq
    port map (pc => qmvagjnfi, gvwe => fwc);
  bgcnbpn : entity work.o
    port map (esmhgzf => zlya, xfxosmw => jmcc, kdjfwoz => sruup, fd => qyu);
  t : entity work.o
    port map (esmhgzf => qgbbzmwy, xfxosmw => dfeslbzmfg, kdjfwoz => lf, fd => pkjduhlyne);
end lvkc;

entity cbzxtihyid is
  port (ki : linkage real; apesbqdau : inout real);
end cbzxtihyid;

library ieee;
use ieee.std_logic_1164.all;

architecture ghdqr of cbzxtihyid is
  signal rehjhxovks : severity_level;
  signal sejuuvtqt : real;
  signal sdqbvzmqmc : bit;
  signal xqpby : boolean;
  signal afnmeejgd : std_logic;
  signal ud : bit;
  signal xjugdjietc : boolean;
  signal loeunia : std_logic;
  signal ghcrusxtz : severity_level;
  signal uixlpuyymu : real;
begin
  nylsq : entity work.vcxieq
    port map (pc => uixlpuyymu, gvwe => ghcrusxtz);
  bucaebyt : entity work.cunckgeas
    port map (sruup => loeunia, zku => xjugdjietc, hrng => ud);
  ibvjmuz : entity work.cunckgeas
    port map (sruup => afnmeejgd, zku => xqpby, hrng => sdqbvzmqmc);
  jrb : entity work.vcxieq
    port map (pc => sejuuvtqt, gvwe => rehjhxovks);
  
  -- Single-driven assignments
  apesbqdau <= uixlpuyymu;
  
  -- Multi-driven assignments
  loeunia <= 'X';
end ghdqr;



-- Seed after: 6705731477088230787,13843488114570579517
