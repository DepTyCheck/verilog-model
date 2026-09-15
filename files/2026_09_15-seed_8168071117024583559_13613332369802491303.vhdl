-- Seed: 8168071117024583559,13613332369802491303

entity v is
  port (tuqotdo : linkage severity_level; grtnqmsib : out time; fuzglen : linkage real; gocwakvaak : inout severity_level);
end v;

architecture ayvvgjx of v is
  
begin
  
end ayvvgjx;

entity gwnfborn is
  port (x : linkage integer);
end gwnfborn;

architecture fplzbbkr of gwnfborn is
  signal yhiz : severity_level;
  signal vmubv : real;
  signal liwph : time;
  signal wywcj : severity_level;
  signal a : severity_level;
  signal zy : real;
  signal vgrwkbner : time;
  signal fajvwgxbkq : severity_level;
  signal zra : severity_level;
  signal iysuf : real;
  signal qazkufhfj : time;
  signal shqqeoow : severity_level;
  signal hd : severity_level;
  signal nojlxmgfh : real;
  signal sgav : time;
  signal i : severity_level;
begin
  ulw : entity work.v
    port map (tuqotdo => i, grtnqmsib => sgav, fuzglen => nojlxmgfh, gocwakvaak => hd);
  eztvzlnl : entity work.v
    port map (tuqotdo => shqqeoow, grtnqmsib => qazkufhfj, fuzglen => iysuf, gocwakvaak => zra);
  qrqal : entity work.v
    port map (tuqotdo => fajvwgxbkq, grtnqmsib => vgrwkbner, fuzglen => zy, gocwakvaak => a);
  q : entity work.v
    port map (tuqotdo => wywcj, grtnqmsib => liwph, fuzglen => vmubv, gocwakvaak => yhiz);
end fplzbbkr;

library ieee;
use ieee.std_logic_1164.all;

entity sjdqbdjjbu is
  port (jjqolat : linkage integer; nuyexbjlht : out std_logic_vector(0 to 3); cgd : linkage integer; niwdye : inout integer);
end sjdqbdjjbu;

architecture yhr of sjdqbdjjbu is
  signal olwb : severity_level;
  signal ekauzmjlvy : real;
  signal cbgr : time;
  signal rbavpq : severity_level;
  signal ra : severity_level;
  signal wqg : real;
  signal kyj : time;
  signal m : severity_level;
  signal bppehfegjo : integer;
begin
  kwqsolbt : entity work.gwnfborn
    port map (x => bppehfegjo);
  cpjxgx : entity work.v
    port map (tuqotdo => m, grtnqmsib => kyj, fuzglen => wqg, gocwakvaak => ra);
  apteivkihy : entity work.v
    port map (tuqotdo => rbavpq, grtnqmsib => cbgr, fuzglen => ekauzmjlvy, gocwakvaak => olwb);
  
  -- Single-driven assignments
  niwdye <= niwdye;
  
  -- Multi-driven assignments
  nuyexbjlht <= ('Z', 'U', 'W', 'W');
  nuyexbjlht <= ('X', 'H', 'W', 'Z');
  nuyexbjlht <= nuyexbjlht;
  nuyexbjlht <= "1XZ-";
end yhr;



-- Seed after: 7669218138807833890,13613332369802491303
