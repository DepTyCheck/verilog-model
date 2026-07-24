-- Seed: 12740118636841325462,16461708287571398341

entity bhd is
  port (zkz : inout boolean_vector(3 to 0); tdexzxox : linkage boolean; varlkkv : buffer boolean_vector(4 downto 0); qhgamzsi : in time);
end bhd;

architecture kzekgs of bhd is
  
begin
  
end kzekgs;

entity ijktqg is
  port (cjr : linkage integer; yu : buffer severity_level; epexpdfn : in bit_vector(0 downto 3); rigyarfx : buffer real);
end ijktqg;

architecture ed of ijktqg is
  signal ejwuiy : time;
  signal mfwfbmwy : boolean_vector(4 downto 0);
  signal jnonv : boolean;
  signal vitijxa : boolean_vector(3 to 0);
  signal jnjo : boolean_vector(4 downto 0);
  signal az : boolean;
  signal t : boolean_vector(3 to 0);
  signal kjhudxj : boolean_vector(4 downto 0);
  signal oudvkw : boolean;
  signal atpiblq : boolean_vector(3 to 0);
  signal fyiwgsnyvy : time;
  signal kbt : boolean_vector(4 downto 0);
  signal ehrgzj : boolean;
  signal ety : boolean_vector(3 to 0);
begin
  eenmkbxj : entity work.bhd
    port map (zkz => ety, tdexzxox => ehrgzj, varlkkv => kbt, qhgamzsi => fyiwgsnyvy);
  xqds : entity work.bhd
    port map (zkz => atpiblq, tdexzxox => oudvkw, varlkkv => kjhudxj, qhgamzsi => fyiwgsnyvy);
  cblmqut : entity work.bhd
    port map (zkz => t, tdexzxox => az, varlkkv => jnjo, qhgamzsi => fyiwgsnyvy);
  kvelgb : entity work.bhd
    port map (zkz => vitijxa, tdexzxox => jnonv, varlkkv => mfwfbmwy, qhgamzsi => ejwuiy);
  
  -- Single-driven assignments
  rigyarfx <= 032.1204;
  ejwuiy <= fyiwgsnyvy;
  fyiwgsnyvy <= 1 hr;
  yu <= FAILURE;
end ed;



-- Seed after: 11358562660402581675,16461708287571398341
