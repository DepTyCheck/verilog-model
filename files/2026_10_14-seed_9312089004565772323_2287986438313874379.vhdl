-- Seed: 9312089004565772323,2287986438313874379

library ieee;
use ieee.std_logic_1164.all;

entity vv is
  port (rppgfxzgz : buffer bit; dusiwuxl : out std_logic_vector(3 downto 3); pexnpct : in integer; irrebuonl : inout time);
end vv;

architecture pkagdou of vv is
  
begin
  -- Single-driven assignments
  irrebuonl <= irrebuonl;
  rppgfxzgz <= rppgfxzgz;
  
  -- Multi-driven assignments
  dusiwuxl <= (others => 'L');
  dusiwuxl <= dusiwuxl;
  dusiwuxl <= "Z";
end pkagdou;

library ieee;
use ieee.std_logic_1164.all;

entity pklxld is
  port (ce : in std_logic_vector(0 to 2); kaufqgh : buffer std_logic; xxuzi : linkage time; zhgiiiob : out boolean_vector(0 to 3));
end pklxld;

library ieee;
use ieee.std_logic_1164.all;

architecture xqvud of pklxld is
  signal agra : time;
  signal v : std_logic_vector(3 downto 3);
  signal ckumk : bit;
  signal jak : time;
  signal bho : integer;
  signal dbe : bit;
  signal pvffr : time;
  signal adlkkweu : integer;
  signal lldnlw : std_logic_vector(3 downto 3);
  signal cnylikraz : bit;
begin
  azuzrvbw : entity work.vv
    port map (rppgfxzgz => cnylikraz, dusiwuxl => lldnlw, pexnpct => adlkkweu, irrebuonl => pvffr);
  rhx : entity work.vv
    port map (rppgfxzgz => dbe, dusiwuxl => lldnlw, pexnpct => bho, irrebuonl => jak);
  o : entity work.vv
    port map (rppgfxzgz => ckumk, dusiwuxl => v, pexnpct => adlkkweu, irrebuonl => agra);
  
  -- Single-driven assignments
  zhgiiiob <= (FALSE, FALSE, TRUE, FALSE);
  bho <= adlkkweu;
  adlkkweu <= 4_0_4_4;
  
  -- Multi-driven assignments
  kaufqgh <= 'W';
  v <= "U";
  kaufqgh <= '-';
end xqvud;



-- Seed after: 15256037279931001113,2287986438313874379
