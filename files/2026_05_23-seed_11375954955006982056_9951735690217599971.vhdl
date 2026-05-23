-- Seed: 11375954955006982056,9951735690217599971

library ieee;
use ieee.std_logic_1164.all;

entity pcgnorm is
  port (rl : in real; vznuixihu : linkage std_logic_vector(3 to 1); m : buffer std_logic_vector(3 to 0); ihoxtzwqjr : linkage std_logic_vector(3 to 2));
end pcgnorm;



architecture gboyrfu of pcgnorm is
  
begin
  
end gboyrfu;



entity ajtelh is
  port (klgdpqlbhr : linkage integer; u : out integer; l : inout integer; qqywabqea : out real);
end ajtelh;

library ieee;
use ieee.std_logic_1164.all;

architecture djgtk of ajtelh is
  signal ldfl : std_logic_vector(3 to 2);
  signal fqkjvkdv : real;
  signal qhpgfisp : std_logic_vector(3 to 1);
  signal iyqveh : std_logic_vector(3 to 0);
  signal lif : std_logic_vector(3 to 2);
  signal gdvtz : real;
begin
  usp : entity work.pcgnorm
    port map (rl => gdvtz, vznuixihu => lif, m => lif, ihoxtzwqjr => lif);
  eozupa : entity work.pcgnorm
    port map (rl => qqywabqea, vznuixihu => iyqveh, m => iyqveh, ihoxtzwqjr => qhpgfisp);
  pskibeu : entity work.pcgnorm
    port map (rl => fqkjvkdv, vznuixihu => qhpgfisp, m => lif, ihoxtzwqjr => lif);
  ijdggxxchg : entity work.pcgnorm
    port map (rl => qqywabqea, vznuixihu => ldfl, m => ldfl, ihoxtzwqjr => ldfl);
end djgtk;

library ieee;
use ieee.std_logic_1164.all;

entity rtk is
  port (ccprads : linkage std_logic_vector(2 to 3); adjrufrzfq : linkage integer);
end rtk;

library ieee;
use ieee.std_logic_1164.all;

architecture t of rtk is
  signal hjcqjga : real;
  signal zqnj : integer;
  signal mhwsirxnkr : integer;
  signal uc : std_logic_vector(3 to 2);
  signal lniw : std_logic_vector(3 to 0);
  signal itdzz : std_logic_vector(3 to 1);
  signal jgchck : std_logic_vector(3 to 2);
  signal xynggfy : std_logic_vector(3 to 0);
  signal kpbchv : std_logic_vector(3 to 1);
  signal esbvkcnjb : std_logic_vector(3 to 2);
  signal dio : std_logic_vector(3 to 0);
  signal nnkgt : std_logic_vector(3 to 1);
  signal scu : real;
begin
  unnyw : entity work.pcgnorm
    port map (rl => scu, vznuixihu => nnkgt, m => dio, ihoxtzwqjr => esbvkcnjb);
  zvnrxqhh : entity work.pcgnorm
    port map (rl => scu, vznuixihu => kpbchv, m => xynggfy, ihoxtzwqjr => jgchck);
  pgrwhrt : entity work.pcgnorm
    port map (rl => scu, vznuixihu => itdzz, m => lniw, ihoxtzwqjr => uc);
  o : entity work.ajtelh
    port map (klgdpqlbhr => mhwsirxnkr, u => mhwsirxnkr, l => zqnj, qqywabqea => hjcqjga);
end t;



-- Seed after: 12272864635695210363,9951735690217599971
