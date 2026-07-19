-- Seed: 12944683170394910344,5511103086789671269

library ieee;
use ieee.std_logic_1164.all;

entity moazbqqaj is
  port (qpr : buffer time; xxrraaop : inout boolean; kzqjwyoq : buffer std_logic; lmlty : out std_logic);
end moazbqqaj;

architecture dkptamyv of moazbqqaj is
  
begin
  -- Single-driven assignments
  xxrraaop <= TRUE;
end dkptamyv;

entity gx is
  port (fddmtmwt : inout time; aahuxqxdh : buffer time_vector(1 downto 0); cq : out bit);
end gx;

architecture iis of gx is
  
begin
  -- Single-driven assignments
  fddmtmwt <= fddmtmwt;
  aahuxqxdh <= (2#0_0# ps, 8#3# ms);
  cq <= '0';
end iis;

library ieee;
use ieee.std_logic_1164.all;

entity kwwztegs is
  port (ahcbcclszh : in std_logic_vector(0 to 2));
end kwwztegs;

library ieee;
use ieee.std_logic_1164.all;

architecture ziswwuzrh of kwwztegs is
  signal li : boolean;
  signal vezgbu : time;
  signal rk : std_logic;
  signal uxlpys : std_logic;
  signal gprvr : boolean;
  signal ax : time;
  signal svwrpehyw : std_logic;
  signal aum : boolean;
  signal rfja : time;
begin
  jzr : entity work.moazbqqaj
    port map (qpr => rfja, xxrraaop => aum, kzqjwyoq => svwrpehyw, lmlty => svwrpehyw);
  nkhrmn : entity work.moazbqqaj
    port map (qpr => ax, xxrraaop => gprvr, kzqjwyoq => uxlpys, lmlty => rk);
  q : entity work.moazbqqaj
    port map (qpr => vezgbu, xxrraaop => li, kzqjwyoq => svwrpehyw, lmlty => uxlpys);
end ziswwuzrh;



-- Seed after: 17695380868902180407,5511103086789671269
