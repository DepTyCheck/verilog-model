-- Seed: 1246870039857588527,13903879141658024201

library ieee;
use ieee.std_logic_1164.all;

entity mnocaqi is
  port (ajr : inout std_logic; mjhaq : linkage bit_vector(3 to 0); hrrowrrgti : buffer std_logic; ctpdzjqjml : linkage integer);
end mnocaqi;



architecture hkovktz of mnocaqi is
  
begin
  
end hkovktz;



entity btrcq is
  port (qj : linkage integer_vector(1 to 2));
end btrcq;

library ieee;
use ieee.std_logic_1164.all;

architecture fzr of btrcq is
  signal dskwkhpzoc : std_logic;
  signal xdr : integer;
  signal k : bit_vector(3 to 0);
  signal tlgtyygf : std_logic;
begin
  upqqg : entity work.mnocaqi
    port map (ajr => tlgtyygf, mjhaq => k, hrrowrrgti => tlgtyygf, ctpdzjqjml => xdr);
  uhhrshv : entity work.mnocaqi
    port map (ajr => dskwkhpzoc, mjhaq => k, hrrowrrgti => tlgtyygf, ctpdzjqjml => xdr);
end fzr;

library ieee;
use ieee.std_logic_1164.all;

entity pmbhhrd is
  port (wmrp : out integer; wesgao : buffer std_logic; bicq : inout std_logic; o : linkage std_logic_vector(2 to 0));
end pmbhhrd;



architecture rsx of pmbhhrd is
  signal oln : integer_vector(1 to 2);
  signal hbxjkyu : integer_vector(1 to 2);
  signal ebwn : bit_vector(3 to 0);
begin
  y : entity work.mnocaqi
    port map (ajr => wesgao, mjhaq => ebwn, hrrowrrgti => bicq, ctpdzjqjml => wmrp);
  eap : entity work.btrcq
    port map (qj => hbxjkyu);
  uuvpeecqzs : entity work.btrcq
    port map (qj => oln);
  psayal : entity work.btrcq
    port map (qj => hbxjkyu);
end rsx;



-- Seed after: 2692937584408270044,13903879141658024201
