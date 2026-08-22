-- Seed: 10662244620783293396,5805648483995786113

library ieee;
use ieee.std_logic_1164.all;

entity lguev is
  port (nop : out string(4 downto 2); coalvtp : buffer time_vector(2 to 0); b : buffer boolean; nk : inout std_logic_vector(0 to 3));
end lguev;

architecture hlxwi of lguev is
  
begin
  -- Single-driven assignments
  b <= b;
  nop <= nop;
  coalvtp <= coalvtp;
end hlxwi;

entity gfxd is
  port (vkthcrkraw : inout real_vector(3 downto 1); gxnni : inout integer; ctcyquy : buffer string(4 downto 1));
end gfxd;

architecture hhf of gfxd is
  
begin
  -- Single-driven assignments
  gxnni <= gxnni;
end hhf;

entity mgqhpdx is
  port (ilohbjifc : inout string(4 to 2); luyrmveub : out integer);
end mgqhpdx;

library ieee;
use ieee.std_logic_1164.all;

architecture irwniul of mgqhpdx is
  signal i : string(4 downto 1);
  signal lug : integer;
  signal phigiyw : real_vector(3 downto 1);
  signal bknuoor : std_logic_vector(0 to 3);
  signal r : boolean;
  signal ouq : time_vector(2 to 0);
  signal mrtrkpjn : string(4 downto 2);
begin
  ffahcbt : entity work.lguev
    port map (nop => mrtrkpjn, coalvtp => ouq, b => r, nk => bknuoor);
  mpzeabwav : entity work.gfxd
    port map (vkthcrkraw => phigiyw, gxnni => lug, ctcyquy => i);
  
  -- Single-driven assignments
  luyrmveub <= lug;
end irwniul;



-- Seed after: 8221830314105761469,5805648483995786113
