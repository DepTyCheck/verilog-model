-- Seed: 4253024331114961471,3108530264173481209

library ieee;
use ieee.std_logic_1164.all;

entity jvtctieapo is
  port (ilhwaok : out time_vector(4 to 0); orrvd : linkage std_logic_vector(1 to 2));
end jvtctieapo;

architecture crc of jvtctieapo is
  
begin
  -- Single-driven assignments
  ilhwaok <= (others => 0 ns);
end crc;

entity fdnytp is
  port (nnczwnsdof : linkage integer; hrjyk : out real; ah : buffer integer_vector(1 downto 3); pplfzhqjel : in boolean);
end fdnytp;

library ieee;
use ieee.std_logic_1164.all;

architecture p of fdnytp is
  signal ogohzkvgyj : std_logic_vector(1 to 2);
  signal ffqk : time_vector(4 to 0);
  signal thjzwsah : std_logic_vector(1 to 2);
  signal oxfpwerus : time_vector(4 to 0);
  signal r : std_logic_vector(1 to 2);
  signal tdvdoztkhr : time_vector(4 to 0);
begin
  oaqkannfo : entity work.jvtctieapo
    port map (ilhwaok => tdvdoztkhr, orrvd => r);
  ynm : entity work.jvtctieapo
    port map (ilhwaok => oxfpwerus, orrvd => thjzwsah);
  nmcbo : entity work.jvtctieapo
    port map (ilhwaok => ffqk, orrvd => ogohzkvgyj);
  
  -- Multi-driven assignments
  thjzwsah <= "UZ";
  thjzwsah <= ('H', '1');
  r <= "Z0";
end p;

entity wdv is
  port (wyovfzxw : linkage real; ey : linkage time);
end wdv;

library ieee;
use ieee.std_logic_1164.all;

architecture ybskv of wdv is
  signal lraco : time_vector(4 to 0);
  signal dq : std_logic_vector(1 to 2);
  signal spddewcuhk : time_vector(4 to 0);
  signal kyu : time_vector(4 to 0);
  signal wya : std_logic_vector(1 to 2);
  signal kknj : time_vector(4 to 0);
begin
  bg : entity work.jvtctieapo
    port map (ilhwaok => kknj, orrvd => wya);
  k : entity work.jvtctieapo
    port map (ilhwaok => kyu, orrvd => wya);
  axjmbdvvz : entity work.jvtctieapo
    port map (ilhwaok => spddewcuhk, orrvd => dq);
  czua : entity work.jvtctieapo
    port map (ilhwaok => lraco, orrvd => dq);
  
  -- Multi-driven assignments
  wya <= ('1', 'H');
end ybskv;



-- Seed after: 13046765611480823828,3108530264173481209
