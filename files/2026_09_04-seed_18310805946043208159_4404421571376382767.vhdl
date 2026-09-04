-- Seed: 18310805946043208159,4404421571376382767

entity gnlbjvef is
  port (uepgf : buffer time_vector(2 to 3));
end gnlbjvef;

architecture nutldqxn of gnlbjvef is
  
begin
  
end nutldqxn;

library ieee;
use ieee.std_logic_1164.all;

entity g is
  port (gnje : buffer std_logic_vector(2 downto 1); c : linkage std_logic_vector(4 to 2); fk : linkage time);
end g;

architecture ksa of g is
  signal d : time_vector(2 to 3);
  signal ditnoj : time_vector(2 to 3);
  signal j : time_vector(2 to 3);
begin
  yvhnlety : entity work.gnlbjvef
    port map (uepgf => j);
  afng : entity work.gnlbjvef
    port map (uepgf => ditnoj);
  snlyvntt : entity work.gnlbjvef
    port map (uepgf => d);
  
  -- Multi-driven assignments
  gnje <= ('L', '1');
  gnje <= ('1', 'L');
end ksa;

entity ruewidjlqc is
  port (bli : inout time; c : inout time);
end ruewidjlqc;

library ieee;
use ieee.std_logic_1164.all;

architecture kwpijxq of ruewidjlqc is
  signal gbdd : time_vector(2 to 3);
  signal m : time_vector(2 to 3);
  signal vrkvw : std_logic_vector(4 to 2);
  signal d : std_logic_vector(2 downto 1);
begin
  nd : entity work.g
    port map (gnje => d, c => vrkvw, fk => bli);
  ezfwmeungo : entity work.gnlbjvef
    port map (uepgf => m);
  fcdhfk : entity work.gnlbjvef
    port map (uepgf => gbdd);
  
  -- Single-driven assignments
  c <= 4 min;
end kwpijxq;

entity fwmnrdgfw is
  port (grfnesh : buffer real);
end fwmnrdgfw;

architecture rfas of fwmnrdgfw is
  signal qtznvhiwuk : time_vector(2 to 3);
  signal gvcfokm : time_vector(2 to 3);
begin
  jog : entity work.gnlbjvef
    port map (uepgf => gvcfokm);
  jlq : entity work.gnlbjvef
    port map (uepgf => qtznvhiwuk);
end rfas;



-- Seed after: 7964663059440693995,4404421571376382767
