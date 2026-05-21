-- Seed: 3306044556681204949,11460338385561364835

library ieee;
use ieee.std_logic_1164.all;

entity psebdn is
  port (qmcvqacv : out time; daozlnf : in boolean; fguie : buffer std_logic; jzqt : out severity_level);
end psebdn;



architecture srwvgpvho of psebdn is
  
begin
  
end srwvgpvho;



entity wcng is
  port (tly : out time; fwvozedjus : inout integer);
end wcng;

library ieee;
use ieee.std_logic_1164.all;

architecture kdomx of wcng is
  signal qcum : severity_level;
  signal eyajyfnmtr : boolean;
  signal bl : severity_level;
  signal khgwkiw : std_logic;
  signal wxg : boolean;
  signal pmlusdxju : time;
begin
  q : entity work.psebdn
    port map (qmcvqacv => pmlusdxju, daozlnf => wxg, fguie => khgwkiw, jzqt => bl);
  l : entity work.psebdn
    port map (qmcvqacv => tly, daozlnf => eyajyfnmtr, fguie => khgwkiw, jzqt => qcum);
end kdomx;

library ieee;
use ieee.std_logic_1164.all;

entity hyrrp is
  port (cp : linkage real; zpjceb : buffer integer; jln : linkage real; jfpucwnq : out std_logic);
end hyrrp;



architecture yrkg of hyrrp is
  
begin
  
end yrkg;

library ieee;
use ieee.std_logic_1164.all;

entity puhimcml is
  port (iwg : inout time; xvbqreqyae : buffer real; rdwqm : out std_logic; lntmj : inout time);
end puhimcml;

library ieee;
use ieee.std_logic_1164.all;

architecture ibllwjqmos of puhimcml is
  signal wndc : integer;
  signal huamlb : real;
  signal ioctoen : integer;
  signal enwubqpvl : time;
  signal liaf : severity_level;
  signal dvxvxq : std_logic;
  signal b : boolean;
begin
  u : entity work.psebdn
    port map (qmcvqacv => lntmj, daozlnf => b, fguie => dvxvxq, jzqt => liaf);
  l : entity work.wcng
    port map (tly => enwubqpvl, fwvozedjus => ioctoen);
  rwpmcqlx : entity work.hyrrp
    port map (cp => huamlb, zpjceb => wndc, jln => xvbqreqyae, jfpucwnq => rdwqm);
end ibllwjqmos;



-- Seed after: 17907603269726419651,11460338385561364835
