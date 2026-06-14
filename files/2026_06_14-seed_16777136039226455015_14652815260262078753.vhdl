-- Seed: 16777136039226455015,14652815260262078753

library ieee;
use ieee.std_logic_1164.all;

entity ajr is
  port (mx : out bit; dzrotpahyn : inout std_logic; dzfxzfnq : buffer std_logic; kz : in bit_vector(2 downto 4));
end ajr;

architecture rpg of ajr is
  
begin
  -- Single-driven assignments
  mx <= '1';
  
  -- Multi-driven assignments
  dzfxzfnq <= 'Z';
  dzrotpahyn <= 'X';
  dzfxzfnq <= 'W';
  dzfxzfnq <= 'H';
end rpg;

entity aa is
  port (x : out real_vector(3 downto 2); pqycwdolsk : inout string(4 downto 5); fxfuhktt : linkage time; idqbvd : buffer boolean);
end aa;

library ieee;
use ieee.std_logic_1164.all;

architecture vlrtumdxa of aa is
  signal aleo : bit_vector(2 downto 4);
  signal lemkduprl : bit;
  signal yrhhco : bit_vector(2 downto 4);
  signal cbhtz : std_logic;
  signal kcwk : bit;
  signal rr : bit_vector(2 downto 4);
  signal ovgzi : std_logic;
  signal h : bit;
begin
  fdwg : entity work.ajr
    port map (mx => h, dzrotpahyn => ovgzi, dzfxzfnq => ovgzi, kz => rr);
  trli : entity work.ajr
    port map (mx => kcwk, dzrotpahyn => cbhtz, dzfxzfnq => ovgzi, kz => yrhhco);
  yjvujemzgt : entity work.ajr
    port map (mx => lemkduprl, dzrotpahyn => cbhtz, dzfxzfnq => ovgzi, kz => aleo);
  
  -- Single-driven assignments
  yrhhco <= (others => '0');
  aleo <= (others => '0');
  idqbvd <= TRUE;
  x <= (0_0_1_0.2213, 3.143);
  
  -- Multi-driven assignments
  ovgzi <= 'Z';
  ovgzi <= '-';
end vlrtumdxa;

library ieee;
use ieee.std_logic_1164.all;

entity ckhmi is
  port (iltfoby : buffer std_logic; gr : inout bit; xxynilfdi : in integer);
end ckhmi;

library ieee;
use ieee.std_logic_1164.all;

architecture ixqw of ckhmi is
  signal aeq : boolean;
  signal msgcwyx : time;
  signal fsntrxa : string(4 downto 5);
  signal gng : real_vector(3 downto 2);
  signal koavjtnxy : std_logic;
  signal oqcnzs : bit_vector(2 downto 4);
  signal dvvv : std_logic;
  signal vxm : bit;
begin
  vtyrd : entity work.ajr
    port map (mx => vxm, dzrotpahyn => dvvv, dzfxzfnq => iltfoby, kz => oqcnzs);
  uhqypwzquu : entity work.ajr
    port map (mx => gr, dzrotpahyn => dvvv, dzfxzfnq => koavjtnxy, kz => oqcnzs);
  ofvsf : entity work.aa
    port map (x => gng, pqycwdolsk => fsntrxa, fxfuhktt => msgcwyx, idqbvd => aeq);
  
  -- Single-driven assignments
  oqcnzs <= (others => '0');
end ixqw;



-- Seed after: 9293509695514013587,14652815260262078753
