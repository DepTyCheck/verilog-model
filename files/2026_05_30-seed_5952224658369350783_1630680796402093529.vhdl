-- Seed: 5952224658369350783,1630680796402093529

library ieee;
use ieee.std_logic_1164.all;

entity p is
  port (udb : out real; whr : in std_logic; ccu : in real_vector(4 downto 1); zfojcc : in real);
end p;



architecture svriiswn of p is
  
begin
  
end svriiswn;

library ieee;
use ieee.std_logic_1164.all;

entity yq is
  port (wrj : in real; aunrrdpl : in std_logic_vector(0 to 2));
end yq;

library ieee;
use ieee.std_logic_1164.all;

architecture qobhxix of yq is
  signal cj : std_logic;
  signal scbtnkf : real;
  signal cvaeh : std_logic;
  signal tclgqsnyq : real;
  signal vlqqmhdt : real_vector(4 downto 1);
  signal edsnilyu : std_logic;
  signal wbk : real;
begin
  jklpi : entity work.p
    port map (udb => wbk, whr => edsnilyu, ccu => vlqqmhdt, zfojcc => tclgqsnyq);
  qlppsdo : entity work.p
    port map (udb => tclgqsnyq, whr => cvaeh, ccu => vlqqmhdt, zfojcc => scbtnkf);
  fdapgiky : entity work.p
    port map (udb => scbtnkf, whr => cj, ccu => vlqqmhdt, zfojcc => wbk);
end qobhxix;

library ieee;
use ieee.std_logic_1164.all;

entity u is
  port (bybjy : inout real; catriamfs : buffer boolean_vector(2 downto 3); yki : inout std_logic_vector(1 downto 1); ehmey : inout time_vector(2 to 3));
end u;

library ieee;
use ieee.std_logic_1164.all;

architecture yakitbc of u is
  signal qngahwqju : std_logic_vector(0 to 2);
  signal jkjegkx : real;
  signal cov : real;
  signal pcnvwhbh : real;
  signal tovxc : real;
  signal boxuiaegm : real_vector(4 downto 1);
  signal cdwkaznfd : std_logic;
  signal zfaruptl : real_vector(4 downto 1);
  signal hudhnceae : std_logic;
  signal dlysevlpt : real;
begin
  qwlqqkv : entity work.p
    port map (udb => dlysevlpt, whr => hudhnceae, ccu => zfaruptl, zfojcc => bybjy);
  crw : entity work.p
    port map (udb => bybjy, whr => cdwkaznfd, ccu => boxuiaegm, zfojcc => tovxc);
  jzfrpxpd : entity work.p
    port map (udb => pcnvwhbh, whr => hudhnceae, ccu => boxuiaegm, zfojcc => cov);
  s : entity work.yq
    port map (wrj => jkjegkx, aunrrdpl => qngahwqju);
end yakitbc;



entity ci is
  port (khav : inout time; xthiuc : linkage real);
end ci;

library ieee;
use ieee.std_logic_1164.all;

architecture yyyu of ci is
  signal av : real;
  signal j : real;
  signal buls : real_vector(4 downto 1);
  signal heyq : std_logic;
  signal rzcfg : real;
begin
  wymfsv : entity work.p
    port map (udb => rzcfg, whr => heyq, ccu => buls, zfojcc => j);
  apoi : entity work.p
    port map (udb => av, whr => heyq, ccu => buls, zfojcc => rzcfg);
end yyyu;



-- Seed after: 6028300324867753836,1630680796402093529
