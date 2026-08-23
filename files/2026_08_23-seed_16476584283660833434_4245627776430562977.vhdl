-- Seed: 16476584283660833434,4245627776430562977

library ieee;
use ieee.std_logic_1164.all;

entity jnvg is
  port (err : linkage integer_vector(2 to 0); amdbbhg : linkage integer; tgemfwsz : inout string(1 downto 4); hwb : buffer std_logic_vector(4 to 2));
end jnvg;

architecture vei of jnvg is
  
begin
  
end vei;

library ieee;
use ieee.std_logic_1164.all;

entity lxfyrzb is
  port (gyxrnmztfl : linkage time; ovr : in integer; vnsuzpamy : in std_logic; emxswnyoyv : buffer std_logic_vector(4 to 4));
end lxfyrzb;

architecture atf of lxfyrzb is
  
begin
  -- Multi-driven assignments
  emxswnyoyv <= emxswnyoyv;
  emxswnyoyv <= emxswnyoyv;
  emxswnyoyv <= "U";
end atf;

entity jisew is
  port (fjocjk : out integer; sbdeeo : linkage bit);
end jisew;

library ieee;
use ieee.std_logic_1164.all;

architecture xqdkonmvdh of jisew is
  signal keungob : string(1 downto 4);
  signal cgogh : integer_vector(2 to 0);
  signal zmr : std_logic_vector(4 to 2);
  signal dndm : string(1 downto 4);
  signal urhcrf : integer;
  signal qkseaj : integer_vector(2 to 0);
begin
  bnuhvu : entity work.jnvg
    port map (err => qkseaj, amdbbhg => urhcrf, tgemfwsz => dndm, hwb => zmr);
  urai : entity work.jnvg
    port map (err => cgogh, amdbbhg => fjocjk, tgemfwsz => keungob, hwb => zmr);
  
  -- Multi-driven assignments
  zmr <= (others => '0');
  zmr <= "";
end xqdkonmvdh;

entity yuivbp is
  port (cwbs : inout real; sryqgfgu : in boolean);
end yuivbp;

library ieee;
use ieee.std_logic_1164.all;

architecture hoydimglgb of yuivbp is
  signal txu : std_logic_vector(4 to 2);
  signal sofimp : string(1 downto 4);
  signal isjrxpuc : integer;
  signal szrb : integer_vector(2 to 0);
  signal nrej : std_logic_vector(4 to 2);
  signal bujohcyjr : string(1 downto 4);
  signal roujjbl : integer;
  signal xwewlbf : integer_vector(2 to 0);
  signal i : std_logic_vector(4 to 4);
  signal opynfeif : std_logic;
  signal hj : integer;
  signal lxmuqxlb : time;
  signal hbyjw : bit;
  signal pbmgmw : integer;
begin
  wiiunghtiu : entity work.jisew
    port map (fjocjk => pbmgmw, sbdeeo => hbyjw);
  tfp : entity work.lxfyrzb
    port map (gyxrnmztfl => lxmuqxlb, ovr => hj, vnsuzpamy => opynfeif, emxswnyoyv => i);
  nxvcpiscqp : entity work.jnvg
    port map (err => xwewlbf, amdbbhg => roujjbl, tgemfwsz => bujohcyjr, hwb => nrej);
  lkwe : entity work.jnvg
    port map (err => szrb, amdbbhg => isjrxpuc, tgemfwsz => sofimp, hwb => txu);
  
  -- Single-driven assignments
  cwbs <= 3_4_4.3_4_3;
  hj <= pbmgmw;
end hoydimglgb;



-- Seed after: 6316408353768324392,4245627776430562977
