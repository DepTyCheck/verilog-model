-- Seed: 15806542078791833195,4177195558088809003

entity dgu is
  port (ofnmwnthhr : in time; rmiygkd : linkage boolean; xiatjfcoyd : inout real_vector(3 to 3); oblckpst : buffer time);
end dgu;

architecture kyozvtsy of dgu is
  
begin
  -- Single-driven assignments
  oblckpst <= ofnmwnthhr;
  xiatjfcoyd <= xiatjfcoyd;
end kyozvtsy;

entity xjpmovwb is
  port (kttmzuuq : inout time; qbryknhowv : linkage integer; wmpmesyd : linkage boolean_vector(1 downto 3); ftw : out real);
end xjpmovwb;

architecture wxykmbj of xjpmovwb is
  signal nthlrpcaed : time;
  signal ff : real_vector(3 to 3);
  signal as : boolean;
  signal g : time;
  signal zwffip : time;
  signal ciavzjmqun : real_vector(3 to 3);
  signal vxefhczdbn : boolean;
  signal tcdauscwk : real_vector(3 to 3);
  signal dcitm : boolean;
  signal ym : time;
  signal bmdoqfds : real_vector(3 to 3);
  signal snnt : boolean;
  signal e : time;
begin
  z : entity work.dgu
    port map (ofnmwnthhr => e, rmiygkd => snnt, xiatjfcoyd => bmdoqfds, oblckpst => ym);
  zxjnjzai : entity work.dgu
    port map (ofnmwnthhr => ym, rmiygkd => dcitm, xiatjfcoyd => tcdauscwk, oblckpst => kttmzuuq);
  uah : entity work.dgu
    port map (ofnmwnthhr => e, rmiygkd => vxefhczdbn, xiatjfcoyd => ciavzjmqun, oblckpst => zwffip);
  cpdpek : entity work.dgu
    port map (ofnmwnthhr => g, rmiygkd => as, xiatjfcoyd => ff, oblckpst => nthlrpcaed);
end wxykmbj;

library ieee;
use ieee.std_logic_1164.all;

entity ndbgbjgcqp is
  port (babajbdyy : inout std_logic_vector(3 to 0));
end ndbgbjgcqp;

architecture jdx of ndbgbjgcqp is
  signal evdom : time;
  signal rwertg : real_vector(3 to 3);
  signal bzm : boolean;
  signal srpweyz : time;
  signal fdyo : real_vector(3 to 3);
  signal amornqwzgm : boolean;
  signal pggvnitxq : time;
  signal ah : real;
  signal blxsnogy : boolean_vector(1 downto 3);
  signal lahmlkm : integer;
  signal jky : time;
  signal akzyms : real;
  signal jtdvyszn : boolean_vector(1 downto 3);
  signal nirki : integer;
  signal aaxfptv : time;
begin
  sx : entity work.xjpmovwb
    port map (kttmzuuq => aaxfptv, qbryknhowv => nirki, wmpmesyd => jtdvyszn, ftw => akzyms);
  uxjmnxu : entity work.xjpmovwb
    port map (kttmzuuq => jky, qbryknhowv => lahmlkm, wmpmesyd => blxsnogy, ftw => ah);
  dfaqgjvrye : entity work.dgu
    port map (ofnmwnthhr => pggvnitxq, rmiygkd => amornqwzgm, xiatjfcoyd => fdyo, oblckpst => srpweyz);
  tkb : entity work.dgu
    port map (ofnmwnthhr => aaxfptv, rmiygkd => bzm, xiatjfcoyd => rwertg, oblckpst => evdom);
  
  -- Single-driven assignments
  pggvnitxq <= aaxfptv;
  
  -- Multi-driven assignments
  babajbdyy <= babajbdyy;
  babajbdyy <= babajbdyy;
  babajbdyy <= babajbdyy;
end jdx;



-- Seed after: 11953506430672741820,4177195558088809003
