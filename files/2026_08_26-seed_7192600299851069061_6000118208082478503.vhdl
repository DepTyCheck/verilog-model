-- Seed: 7192600299851069061,6000118208082478503

entity fn is
  port (pqsxdnffha : out real; s : out character; xifurebqc : linkage time_vector(3 to 4));
end fn;

architecture jn of fn is
  
begin
  
end jn;

library ieee;
use ieee.std_logic_1164.all;

entity pohy is
  port (ko : in std_logic_vector(2 downto 3); vxiizt : buffer std_logic_vector(1 to 0); wwg : buffer time);
end pohy;

architecture xnfxkjgy of pohy is
  signal ssytu : time_vector(3 to 4);
  signal ogjyvz : character;
  signal tdlkaso : real;
  signal uv : time_vector(3 to 4);
  signal ucwugcv : character;
  signal ybwvjwp : real;
begin
  pgdgha : entity work.fn
    port map (pqsxdnffha => ybwvjwp, s => ucwugcv, xifurebqc => uv);
  ubby : entity work.fn
    port map (pqsxdnffha => tdlkaso, s => ogjyvz, xifurebqc => ssytu);
  
  -- Single-driven assignments
  wwg <= wwg;
  
  -- Multi-driven assignments
  vxiizt <= (others => '0');
end xnfxkjgy;

library ieee;
use ieee.std_logic_1164.all;

entity pjhdkqkqu is
  port (oegn : in real_vector(1 downto 1); ui : buffer std_logic_vector(0 downto 2); etdvdsv : linkage std_logic; lajlkrqdbb : linkage time);
end pjhdkqkqu;

architecture deaesoijd of pjhdkqkqu is
  signal gcnduwniy : time_vector(3 to 4);
  signal lezjlnwhe : character;
  signal cll : real;
begin
  rjxymltsac : entity work.fn
    port map (pqsxdnffha => cll, s => lezjlnwhe, xifurebqc => gcnduwniy);
end deaesoijd;

entity wa is
  port (tdr : in time; ihmhyajoob : out time);
end wa;

library ieee;
use ieee.std_logic_1164.all;

architecture yzecfe of wa is
  signal vk : std_logic_vector(1 to 0);
  signal miqjzbboyl : std_logic_vector(2 downto 3);
begin
  hj : entity work.pohy
    port map (ko => miqjzbboyl, vxiizt => vk, wwg => ihmhyajoob);
  
  -- Multi-driven assignments
  miqjzbboyl <= miqjzbboyl;
  miqjzbboyl <= vk;
  vk <= (others => '0');
  vk <= (others => '0');
end yzecfe;



-- Seed after: 9971578763986677644,6000118208082478503
