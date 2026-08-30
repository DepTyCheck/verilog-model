-- Seed: 7566852250302803977,4080032123900078489

library ieee;
use ieee.std_logic_1164.all;

entity ahnxgl is
  port (dwhngfmzc : inout time; tybre : in std_logic_vector(1 to 3); rtkfrewwt : linkage time_vector(4 to 1));
end ahnxgl;

architecture zkti of ahnxgl is
  
begin
  
end zkti;

entity ob is
  port (ktajmynlf : buffer integer);
end ob;

library ieee;
use ieee.std_logic_1164.all;

architecture ozpvtp of ob is
  signal fjyhow : time_vector(4 to 1);
  signal pk : std_logic_vector(1 to 3);
  signal opalic : time;
  signal jv : time_vector(4 to 1);
  signal fwlxdorzh : std_logic_vector(1 to 3);
  signal aolq : time;
begin
  hxgv : entity work.ahnxgl
    port map (dwhngfmzc => aolq, tybre => fwlxdorzh, rtkfrewwt => jv);
  nsfpfod : entity work.ahnxgl
    port map (dwhngfmzc => opalic, tybre => pk, rtkfrewwt => fjyhow);
  
  -- Single-driven assignments
  ktajmynlf <= 42;
  
  -- Multi-driven assignments
  fwlxdorzh <= ('1', 'Z', 'L');
  fwlxdorzh <= fwlxdorzh;
  pk <= fwlxdorzh;
  fwlxdorzh <= ('X', 'U', '0');
end ozpvtp;

library ieee;
use ieee.std_logic_1164.all;

entity fnrvhrbai is
  port (lcqzfdf : inout std_logic);
end fnrvhrbai;

architecture iehgqo of fnrvhrbai is
  
begin
  -- Multi-driven assignments
  lcqzfdf <= lcqzfdf;
end iehgqo;

entity jsd is
  port (rhxvsjgj : in boolean_vector(0 downto 1));
end jsd;

library ieee;
use ieee.std_logic_1164.all;

architecture lzwr of jsd is
  signal kixi : integer;
  signal g : integer;
  signal olqzwazge : time_vector(4 to 1);
  signal wqxkaxzn : std_logic_vector(1 to 3);
  signal sroe : time;
begin
  lxqnv : entity work.ahnxgl
    port map (dwhngfmzc => sroe, tybre => wqxkaxzn, rtkfrewwt => olqzwazge);
  dfnr : entity work.ob
    port map (ktajmynlf => g);
  pvtzkdo : entity work.ob
    port map (ktajmynlf => kixi);
  
  -- Multi-driven assignments
  wqxkaxzn <= ('1', 'L', '-');
  wqxkaxzn <= wqxkaxzn;
end lzwr;



-- Seed after: 11672876893840123074,4080032123900078489
