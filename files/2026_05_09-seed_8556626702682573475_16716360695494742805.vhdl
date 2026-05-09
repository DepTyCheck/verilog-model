-- Seed: 8556626702682573475,16716360695494742805

library ieee;
use ieee.std_logic_1164.all;

entity acitn is
  port (izjeyzywa : linkage time; fryzrc : out real; mjgxtlqf : inout std_logic);
end acitn;



architecture szyeixqz of acitn is
  
begin
  
end szyeixqz;



entity mvon is
  port (klmi : inout real);
end mvon;

library ieee;
use ieee.std_logic_1164.all;

architecture hpl of mvon is
  signal lprvyo : std_logic;
  signal bzrsjxf : real;
  signal h : std_logic;
  signal m : real;
  signal umdsgpayv : time;
begin
  pdkf : entity work.acitn
    port map (izjeyzywa => umdsgpayv, fryzrc => m, mjgxtlqf => h);
  kxzok : entity work.acitn
    port map (izjeyzywa => umdsgpayv, fryzrc => bzrsjxf, mjgxtlqf => lprvyo);
end hpl;

library ieee;
use ieee.std_logic_1164.all;

entity m is
  port (b : inout time; ed : inout integer; bffmk : out severity_level; ioyb : in std_logic);
end m;

library ieee;
use ieee.std_logic_1164.all;

architecture kkapwi of m is
  signal uabbowlr : real;
  signal ftgtmdlp : real;
  signal wzyoxrpwn : std_logic;
  signal yrwfmod : real;
  signal encbhuch : std_logic;
  signal bzcuovd : real;
begin
  gxifm : entity work.acitn
    port map (izjeyzywa => b, fryzrc => bzcuovd, mjgxtlqf => encbhuch);
  mnzxnw : entity work.acitn
    port map (izjeyzywa => b, fryzrc => yrwfmod, mjgxtlqf => wzyoxrpwn);
  xjrfaxvzlp : entity work.mvon
    port map (klmi => ftgtmdlp);
  rifzibqv : entity work.mvon
    port map (klmi => uabbowlr);
end kkapwi;



-- Seed after: 5740589456910404688,16716360695494742805
