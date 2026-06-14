-- Seed: 14009981428822487045,14652815260262078753

entity x is
  port (wqc : out real; ajem : buffer severity_level; fljtriqndq : inout time; hwxhgrfazi : buffer integer);
end x;

architecture gkfcm of x is
  
begin
  -- Single-driven assignments
  ajem <= NOTE;
  hwxhgrfazi <= 8#772#;
end gkfcm;

entity ec is
  port (yxddqd : out boolean_vector(3 downto 0); jzfwe : inout bit_vector(2 to 0));
end ec;

architecture i of ec is
  signal ljlgwyfksz : integer;
  signal whlqi : time;
  signal xmszrnvjx : severity_level;
  signal ti : real;
  signal axeynbzub : integer;
  signal cchvmtzfdt : time;
  signal cu : severity_level;
  signal cfgtf : real;
begin
  xhrxvyb : entity work.x
    port map (wqc => cfgtf, ajem => cu, fljtriqndq => cchvmtzfdt, hwxhgrfazi => axeynbzub);
  nyctin : entity work.x
    port map (wqc => ti, ajem => xmszrnvjx, fljtriqndq => whlqi, hwxhgrfazi => ljlgwyfksz);
  
  -- Single-driven assignments
  jzfwe <= (others => '0');
  yxddqd <= (FALSE, TRUE, FALSE, TRUE);
end i;



-- Seed after: 14189969490119428962,14652815260262078753
