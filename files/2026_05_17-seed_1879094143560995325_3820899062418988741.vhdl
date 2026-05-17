-- Seed: 1879094143560995325,3820899062418988741

library ieee;
use ieee.std_logic_1164.all;

entity sto is
  port (mksrciivjl : out boolean; sigo : buffer time; yecavg : in time; kn : out std_logic);
end sto;



architecture rhyi of sto is
  
begin
  
end rhyi;



entity sfhelona is
  port (fuxhewab : buffer time; k : inout boolean);
end sfhelona;

library ieee;
use ieee.std_logic_1164.all;

architecture t of sfhelona is
  signal avoiyved : std_logic;
  signal ilxzfa : time;
  signal corykgzw : time;
  signal jnylxz : std_logic;
  signal hyz : time;
  signal mxgxhzt : boolean;
  signal v : std_logic;
  signal l : time;
  signal esoxa : boolean;
  signal whgydaud : std_logic;
  signal xu : time;
  signal gpdzvzdyw : boolean;
begin
  m : entity work.sto
    port map (mksrciivjl => gpdzvzdyw, sigo => fuxhewab, yecavg => xu, kn => whgydaud);
  iee : entity work.sto
    port map (mksrciivjl => esoxa, sigo => xu, yecavg => l, kn => v);
  sjz : entity work.sto
    port map (mksrciivjl => mxgxhzt, sigo => hyz, yecavg => hyz, kn => jnylxz);
  j : entity work.sto
    port map (mksrciivjl => k, sigo => corykgzw, yecavg => ilxzfa, kn => avoiyved);
end t;



entity ikpoowv is
  port (ouoennwb : buffer real; tsdgcomw : linkage integer; vpcds : in real; uwsrszer : linkage time);
end ikpoowv;

library ieee;
use ieee.std_logic_1164.all;

architecture wrkhlchjj of ikpoowv is
  signal kn : boolean;
  signal whyfdvcvfb : boolean;
  signal nae : time;
  signal vnkldnto : std_logic;
  signal xzfxsb : time;
  signal uvtsbx : time;
  signal veaevlfltv : boolean;
begin
  qxfsajcdz : entity work.sto
    port map (mksrciivjl => veaevlfltv, sigo => uvtsbx, yecavg => xzfxsb, kn => vnkldnto);
  bvde : entity work.sfhelona
    port map (fuxhewab => nae, k => whyfdvcvfb);
  tlkfj : entity work.sto
    port map (mksrciivjl => kn, sigo => xzfxsb, yecavg => uvtsbx, kn => vnkldnto);
end wrkhlchjj;



entity lh is
  port (ijcsapja : in real);
end lh;

library ieee;
use ieee.std_logic_1164.all;

architecture lx of lh is
  signal mgmoiu : time;
  signal qvx : time;
  signal fmhhwfctp : boolean;
  signal jzeij : time;
  signal kfc : integer;
  signal wrto : real;
  signal zrkanhnh : time;
  signal uuwoamhlb : time;
  signal nojejvwi : boolean;
  signal uojoaxzww : std_logic;
  signal hwyd : time;
  signal btxfbmxs : boolean;
begin
  sjug : entity work.sto
    port map (mksrciivjl => btxfbmxs, sigo => hwyd, yecavg => hwyd, kn => uojoaxzww);
  ap : entity work.sto
    port map (mksrciivjl => nojejvwi, sigo => uuwoamhlb, yecavg => zrkanhnh, kn => uojoaxzww);
  hjoyke : entity work.ikpoowv
    port map (ouoennwb => wrto, tsdgcomw => kfc, vpcds => ijcsapja, uwsrszer => jzeij);
  hysbbh : entity work.sto
    port map (mksrciivjl => fmhhwfctp, sigo => qvx, yecavg => mgmoiu, kn => uojoaxzww);
end lx;



-- Seed after: 7511568376331855033,3820899062418988741
