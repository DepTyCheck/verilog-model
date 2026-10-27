-- Seed: 2420351781176046202,12536735141187299017

library ieee;
use ieee.std_logic_1164.all;

entity zwpod is
  port (omtavprn : buffer time; l : linkage std_logic; gosbszz : linkage real; rj : buffer boolean);
end zwpod;

architecture uawjwjcmi of zwpod is
  
begin
  -- Single-driven assignments
  rj <= TRUE;
  omtavprn <= 8#5# ns;
end uawjwjcmi;

library ieee;
use ieee.std_logic_1164.all;

entity apq is
  port (nmlvd : out std_logic_vector(2 downto 0));
end apq;

library ieee;
use ieee.std_logic_1164.all;

architecture dln of apq is
  signal qlmutlbnr : boolean;
  signal vspgnq : real;
  signal ywqzhqkq : time;
  signal dkkxau : boolean;
  signal y : real;
  signal lm : std_logic;
  signal rmqyib : time;
  signal xtpkygfjxi : boolean;
  signal tnebn : real;
  signal r : std_logic;
  signal m : time;
begin
  rjipwvywx : entity work.zwpod
    port map (omtavprn => m, l => r, gosbszz => tnebn, rj => xtpkygfjxi);
  nyguk : entity work.zwpod
    port map (omtavprn => rmqyib, l => lm, gosbszz => y, rj => dkkxau);
  bf : entity work.zwpod
    port map (omtavprn => ywqzhqkq, l => r, gosbszz => vspgnq, rj => qlmutlbnr);
  
  -- Multi-driven assignments
  nmlvd <= nmlvd;
end dln;



-- Seed after: 452840106654399677,12536735141187299017
