-- Seed: 1083100348279126829,4404421571376382767

entity za is
  port (bady : out boolean; ucp : out time; etbms : inout boolean);
end za;

architecture b of za is
  
begin
  -- Single-driven assignments
  etbms <= FALSE;
  ucp <= ucp;
end b;

library ieee;
use ieee.std_logic_1164.all;

entity gsedotnnh is
  port (qg : linkage real; gpmnzvf : buffer integer_vector(2 to 2); phjkjup : inout std_logic_vector(3 to 3); jf : buffer std_logic_vector(4 downto 2));
end gsedotnnh;

architecture mvfsfyjrd of gsedotnnh is
  signal inblleq : boolean;
  signal xtnlgvjpb : time;
  signal bdj : boolean;
  signal hfpfarsyw : boolean;
  signal cfuosbuud : time;
  signal xjfi : boolean;
  signal r : boolean;
  signal iyg : time;
  signal euxplt : boolean;
begin
  cinspnpj : entity work.za
    port map (bady => euxplt, ucp => iyg, etbms => r);
  wysiiz : entity work.za
    port map (bady => xjfi, ucp => cfuosbuud, etbms => hfpfarsyw);
  yzqzgxgcva : entity work.za
    port map (bady => bdj, ucp => xtnlgvjpb, etbms => inblleq);
  
  -- Single-driven assignments
  gpmnzvf <= (others => 1);
  
  -- Multi-driven assignments
  phjkjup <= phjkjup;
  phjkjup <= (others => 'Z');
  jf <= "ZZX";
end mvfsfyjrd;

entity baebuvkmrn is
  port (xhf : buffer time);
end baebuvkmrn;

library ieee;
use ieee.std_logic_1164.all;

architecture vgwjt of baebuvkmrn is
  signal qiupwis : std_logic_vector(4 downto 2);
  signal unzpu : integer_vector(2 to 2);
  signal dyovqi : real;
  signal pbdarkba : std_logic_vector(4 downto 2);
  signal kps : std_logic_vector(3 to 3);
  signal befyclqt : integer_vector(2 to 2);
  signal ndvuw : real;
begin
  fd : entity work.gsedotnnh
    port map (qg => ndvuw, gpmnzvf => befyclqt, phjkjup => kps, jf => pbdarkba);
  umco : entity work.gsedotnnh
    port map (qg => dyovqi, gpmnzvf => unzpu, phjkjup => kps, jf => qiupwis);
  
  -- Single-driven assignments
  xhf <= 1 ns;
  
  -- Multi-driven assignments
  kps <= kps;
  kps <= kps;
end vgwjt;



-- Seed after: 12396612893931885539,4404421571376382767
