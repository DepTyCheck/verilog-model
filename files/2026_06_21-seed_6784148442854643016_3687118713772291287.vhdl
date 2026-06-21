-- Seed: 6784148442854643016,3687118713772291287

entity qftyhz is
  port (smwv : in time_vector(2 to 3); ovy : buffer time_vector(4 to 0));
end qftyhz;

architecture zxhwz of qftyhz is
  
begin
  -- Single-driven assignments
  ovy <= (others => 0 ns);
end zxhwz;

library ieee;
use ieee.std_logic_1164.all;

entity u is
  port (e : out bit; eo : in time; hfrzaovb : buffer std_logic_vector(0 to 1));
end u;

architecture ovzwc of u is
  signal s : time_vector(4 to 0);
  signal lobatlv : time_vector(2 to 3);
  signal odfxbxgjs : time_vector(4 to 0);
  signal qvupqhh : time_vector(2 to 3);
  signal xavowdy : time_vector(4 to 0);
  signal zdkphsgl : time_vector(4 to 0);
  signal rroyf : time_vector(2 to 3);
begin
  dmqeumq : entity work.qftyhz
    port map (smwv => rroyf, ovy => zdkphsgl);
  hyo : entity work.qftyhz
    port map (smwv => rroyf, ovy => xavowdy);
  zqgqwdjc : entity work.qftyhz
    port map (smwv => qvupqhh, ovy => odfxbxgjs);
  ri : entity work.qftyhz
    port map (smwv => lobatlv, ovy => s);
  
  -- Single-driven assignments
  e <= '1';
  lobatlv <= (16#2959.DA69# ps, 4_3.32332 us);
  rroyf <= (1 hr, 0 sec);
  qvupqhh <= (2_1_1.442 ns, 4 min);
  
  -- Multi-driven assignments
  hfrzaovb <= ('L', 'H');
  hfrzaovb <= ('-', 'U');
end ovzwc;



-- Seed after: 11385663886177193454,3687118713772291287
