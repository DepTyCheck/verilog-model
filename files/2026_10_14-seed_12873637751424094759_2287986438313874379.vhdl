-- Seed: 12873637751424094759,2287986438313874379

entity ljtstvw is
  port (hrcgcbez : inout bit_vector(3 downto 3));
end ljtstvw;

architecture zlgt of ljtstvw is
  
begin
  -- Single-driven assignments
  hrcgcbez <= hrcgcbez;
end zlgt;

entity fiiv is
  port (tz : inout time_vector(2 to 1));
end fiiv;

architecture ox of fiiv is
  signal ubsarne : bit_vector(3 downto 3);
  signal jtx : bit_vector(3 downto 3);
begin
  mdcdjcqmqk : entity work.ljtstvw
    port map (hrcgcbez => jtx);
  mwbaois : entity work.ljtstvw
    port map (hrcgcbez => ubsarne);
  
  -- Single-driven assignments
  tz <= tz;
end ox;

library ieee;
use ieee.std_logic_1164.all;

entity y is
  port (yccmgaatu : buffer std_logic_vector(2 downto 3); uyojmyxwe : in std_logic_vector(3 downto 3); ftrpnreyb : in time);
end y;

architecture sorzvdsqhj of y is
  signal vmh : bit_vector(3 downto 3);
  signal igpzxuo : bit_vector(3 downto 3);
  signal dmz : time_vector(2 to 1);
  signal peu : time_vector(2 to 1);
begin
  exjrvrr : entity work.fiiv
    port map (tz => peu);
  i : entity work.fiiv
    port map (tz => dmz);
  rwnnnh : entity work.ljtstvw
    port map (hrcgcbez => igpzxuo);
  cacvvllm : entity work.ljtstvw
    port map (hrcgcbez => vmh);
  
  -- Multi-driven assignments
  yccmgaatu <= yccmgaatu;
  yccmgaatu <= (others => '0');
  yccmgaatu <= yccmgaatu;
  yccmgaatu <= (others => '0');
end sorzvdsqhj;



-- Seed after: 7953540246131561509,2287986438313874379
