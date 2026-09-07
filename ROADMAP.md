# ocaml-msx 로드맵

MSX2+ 를 OCaml 로 에뮬레이트한다. 최종 목표는 두 개다:

1. 사람이 masc TUI 탭 하나로 게임을 플레이한다.
2. 같은 기계에 Keeper 가 도구로 입력을 넣어 같이 플레이한다.

## 목표 게임 (완료 판정의 기준)

| 게임 | 미디어 | 요구 |
|---|---|---|
| 몽환전사 바리스 | ROM (재믹스 유통) | MSX2, V9938, command engine, 라인 인터럽트 |
| 몽환전사 바리스 2 | 디스크 | + FDC(.DSK) |
| 이스 II | 디스크 2장 | + FM-PAC(YM2413), PAC SRAM 세이브 |
| 룬 마스터 II | 디스크, MSX2, 턴제 보드 RPG | Keeper 동시 플레이 타깃 (P5) |

게임 덤프는 사용자 소관. 이 저장소는 C-BIOS(2-clause BSD)만 실어 배포한다.

## 사다리

| 단계 | 내용 | 완료 기준 |
|---|---|---|
| P0 | Z80 코어 + zexall/zexdoc 하네스 | **완료 (2026-09-07)** — zexall·zexdoc 전 그룹 PASS, 5.76B 명령 / 46.7G T-state. 하네스: `bin/zex.exe`, 단위 벡터 `bin/vectors.exe`, C 오라클 차등 `bin/trace.exe` + `test/zex/diff_driver.c` |
| P1 | MSX1 — V9938 의 TMS 호환 모드, PPI, RAM, C-BIOS | **완료 (2026-09-07)** — XSpelunker(32KB 카트리지, SCREEN 2 + 스프라이트 모드 1) 가 부트 로고 → 타이틀 → LEVEL 1-1 → 게임플레이. 하네스 `bin/boot.exe --roms roms/cbios --cart <rom> --tap-space 340,420`. 남은 틈: 색 0 투명 → R#7 배경색, 조이스틱 방향(커서 키는 키보드 행 8 만), 스프라이트 라인 제한·충돌 플래그, 멀티컬러(SCREEN 3) |
| P2 | MSX2 — command engine, 라인 인터럽트, 128KB VRAM, 팔레트 | 바리스 1·2 (사람이 TUI 로) |
| P3 | 디스크·사운드 — FDC(.DSK), PSG, YM2413, PAC | 이스 II (FM 음악, 세이브) |
| P4 | MSX2+ — V9958 (YJK/YAE 등) | MSX2+ 타이틀 확정 시 지정 |
| P5 | 동시 플레이 — masc 도구 표면, 멀티 클라이언트 키 집합 | 사람과 Keeper 가 같은 세션에서 룬 마스터 II |

통합 순서: 사람이 TUI 로 플레이(P2) → Keeper 참여(P5).

## 코어 계약 (lib/msx.mli)

- 순수: 시간·파일·난수·터미널을 코어가 스스로 읽지 않는다. `step` 호출이 시간을 진행한다.
- 결정론: 같은 상태 + 같은 입력 = 같은 프레임. `serialize`/`restore` 로 구간 재현.
- 프레임은 네이티브 해상도 RGB. 다운샘플과 렌더링은 클라이언트 몫(masc 모자이크/키티).
- 키는 논리 키 주입. `set_key` 는 자리가 있으면 true, 없으면(표 밖 글자, F6 이상) 아무것도 바꾸지 않고 false. 누가 눌렀는지(TUI 사람/keeper)는 코어 밖에서 집합으로 관리한다. 키보드 매트릭스의 정본은 openMSX `unicodemap.int`, 조이스틱 포트(PSG R#14)의 빈 값은 0x3F.
- 실시간 60fps 는 클라이언트의 선택이고, 코어의 기본은 턴제 스텝이다.

## 클라이언트

- `bin/msx_demo` — 터미널 half-block 데모. 지금은 스텁 패턴.
- masc TUI 스펙테이터 탭 — opam path pin 으로 소비. `image_mosaic`/키티 그래이픽으로 중계. ROM 은 `MSX_ROMS`(C-BIOS 디렉터리)·`MSX_CART`(카트리지 한 장) 환경변수.
- keeper 도구 — masc 어휘 라우팅 규칙(variant 라우팅과 `all` 광고 일치)을 따라 등록.

## 원칙

- 하네스 먼저: zexall(P0), 프레임 회귀 덤프(P1~), savestate 왕복 테스트. 렌더·입력 계약은 `dune test` 의 sprite/keyboard/g2 테스트가 포트 경로로 판정한다.
- savestate 포맷은 버전 태그 + 하드컷. 옛 포맷용 변환기·리더를 만들지 않는다.
- 오디오: 코어는 샘플 버퍼만 채운다. 스피커로 내보내는 건 클라이언트 몫.

## 참조

- [V9938 Application Manual (grauw)](https://map.grauw.nl/resources/video/v9938/v9938.xhtml)
- [msx_rs — V9938 command engine 참고 구현 (Rust)](https://github.com/joskwanten/msx_rs)
- [openMSX VDP VRAM 타이밍 문서](https://openmsx.org/vdp-vram-timing/vdp-timing.html)
- [antirez — zexall 통과하는 Z80 (1,200줄 C)](https://antirez.com/news/160), [superzazu/z80](https://github.com/superzazu/z80)
- [C-BIOS](https://cbios.sourceforge.net/)
